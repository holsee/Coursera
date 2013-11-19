package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  
  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("demux example") {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))
    in.setSignal(false)
    c.setSignal(false)
    run
    
    // False and false
    assert(out1.getSignal === false, "and 1a")
    assert(out2.getSignal === false, "and 1b")

    in.setSignal(true)
    run   
    
    // True and false
    assert(out1.getSignal === false, "and 2a")
    assert(out2.getSignal === true, "and 2b")
    
    c.setSignal(true)
    run   
    
    // True and true
    assert(out1.getSignal === true, "and 3a")
    assert(out2.getSignal === false, "and 3b")
    
    in.setSignal(false)
    run   
    
    // False and true
    assert(out1.getSignal === false, "and 4a")
    assert(out2.getSignal === false, "and 4b")
  }
  
}
