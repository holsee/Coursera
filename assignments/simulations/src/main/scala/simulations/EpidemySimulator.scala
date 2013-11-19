package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // Base rules
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val incubationDelay = 6
    val deathDelay = 8
    val deathChance = 0.25
    val immuneDelay = 2
    val healthyDelay = 2

    // Add-ins
    val airplaneMode = false
    val airplaneChance = 0.01

    val mobilityMode = true

    val chosenFewMode = false
    val vipChance = 0.05
  }

  import SimConfig._

  val persons: List[Person] = for (id <- (1 to population).toList) yield new Person(id)

  def personsByField = persons.groupBy(person => (person.row, person.col))

  def neighbours(row: Int, col: Int) = {
    val directions = List((1, 0), (-1, 0), (0, 1), (0, -1))
    for {
      direction <- directions
      nRow = (((row + direction._1) % roomRows) + roomRows) % roomRows //to avoid negatives
      nCol = (((col + direction._2) % roomColumns) + roomColumns) % roomColumns
    } yield (nRow, nCol)
  }

  class Person(val id: Int) {
    val initialInfected = (population * prevalenceRate).toInt
    var infected = id <= initialInfected
    var sick = false
    var immune = id <= (population * vipChance).toInt + initialInfected && id > initialInfected
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def moveAction {

      def infect() {
        val personsInRoom = personsByField.get((row, col))
        if (personsInRoom.isDefined && personsInRoom.get.exists(p => p.infected) && random <= transmissibilityRate) {
          infected = true
          afterDelay(incubationDelay)(sicken)
        }
      }

      if (!dead) {
        if (airplaneMode && random <= airplaneChance) {
          row = randomBelow(roomRows)
          col = randomBelow(roomColumns)
          infect()
        } else {

          val healthyNeighbours = neighbours(row, col).filter(field => {
            val persons = personsByField.get(field)
            !persons.isDefined || !persons.get.exists(p => p.sick || p.dead)
          })

          if (!healthyNeighbours.isEmpty) {
            val neighbour = healthyNeighbours(randomBelow(healthyNeighbours.length))
            row = neighbour._1
            col = neighbour._2
            infect()
          }

        }

        val delay = randomBelow(5) + 1
        afterDelay(if (mobilityMode) (if (sick) delay * 4 else delay * 2) else delay)(moveAction)
      }

    }

    def sicken() {
      sick = true
      afterDelay(deathDelay)(kill)
    }

    def kill() {
      if (random <= deathChance) dead = true
      else afterDelay(immuneDelay)(immunize)
    }

    def immunize() {
      if (!dead) {
        sick = false
        immune = true
        afterDelay(healthyDelay)(recover)
      }
    }

    def recover() {
      if (!dead) {
        infected = false
        immune = false
      }
    }

    if (infected) {
      afterDelay(incubationDelay)(sicken)
    }

    val delay = randomBelow(5) + 1
    afterDelay(if (mobilityMode) (if (sick) delay * 4 else delay * 2) else delay)(moveAction)
  }
}