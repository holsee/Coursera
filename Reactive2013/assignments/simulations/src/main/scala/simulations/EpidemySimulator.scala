package simulations

import math.random

trait DiseaseRules {
    val prevalence = 0.01
    val transmissibility = 0.4
    val chanceOfMortality = 0.25
}

trait InfectionPeriods {
    val becomesContagiousPeriod = 6
    val becomesCriticalPeriod = 8
    val immuneDelay = 2
    val healthyDelay = 2
}

trait AdditionalParameters { 
    val airplaneMode = false
    val airplaneChance = 0.01
    val mobilityMode = true
    val chosenFewMode = false
    val vipChance = 0.05
}

class EpidemySimulator extends Simulator with DiseaseRules with InfectionPeriods with AdditionalParameters {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
  }

  import SimConfig._

  val persons: List[Person] = 
    for (id <- (1 to population).toList) 
      yield new Person(id)

  def neighbours(row: Int, col: Int) = {
    val targets = List((1, 0), (-1, 0), (0, 1), (0, -1))
    for {
      target <- targets
      nRow = (((row + target._1) % roomRows) + roomRows) % roomRows //to avoid negatives
      nCol = (((col + target._2) % roomColumns) + roomColumns) % roomColumns
    } yield (nRow, nCol)
  }

  class Person(val id: Int) {
    val initialInfected = (population * prevalence).toInt
    var infected = id <= initialInfected
    var sick = false
    var immune = id <= (population * vipChance).toInt + initialInfected && id > initialInfected
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def move {
      if (dead)
        return
      
      def tryInfect() {
        val personsInRoom = persons.groupBy(person => (person.row, person.col)).get((row, col))
        if (personsInRoom.isDefined && personsInRoom.get.exists(p => p.infected) && random <= transmissibility) {
          infected = true
          afterDelay(becomesContagiousPeriod)(setSick)
        }
      }
      
      if (airplaneMode && random <= airplaneChance) {
        row = randomBelow(roomRows)
        col = randomBelow(roomColumns)
        tryInfect()
      } else {
        val allNeighbours = neighbours(row, col);
        val neighboursNotInfected = allNeighbours.filter(field => {
          val people = persons.groupBy(person => (person.row, person.col)).get(field)
          !people.isDefined || !people.get.exists(p => p.sick || p.dead)
        });
        if (!neighboursNotInfected.isEmpty) {
          val adjacentNeighbour = neighboursNotInfected(randomBelow(neighboursNotInfected.length))
          row = adjacentNeighbour._1; col = adjacentNeighbour._2
          tryInfect()
        }
      }
      
      val delay = randomBelow(5) + 1
      afterDelay(if (mobilityMode) (if (sick) delay * 4 else delay * 2) else delay)(move)
    }

    def setSick() {
      sick = true
      afterDelay(becomesCriticalPeriod)(tryKill)
    }

    def tryKill() {
      
      def developImmunity() {
        if (dead)
    	  return
    	  
        def recover() {
          if (dead)
            return
        
          infected = false
          immune = false
        }
    
        sick = false; immune = true
        afterDelay(healthyDelay)(recover)
      }
      
      if (random <= chanceOfMortality) 
        dead = true
      else 
        afterDelay(immuneDelay)(developImmunity)
    }

    if (infected) {
      afterDelay(becomesContagiousPeriod)(setSick)
    }

    val delay = randomBelow(5) + 1
    afterDelay(
        if (mobilityMode) 
    		(if (sick) delay * 4 else delay * 2) 
        else delay
    )(move)
  }
}