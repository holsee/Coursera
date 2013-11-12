package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
    e <- arbitrary[Boolean]
  } yield if(e) empty else insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
 
  // Example: Insert single value & findMin
  property("min1") = forAll { x: Int =>
    val h = insert(x, empty)
    findMin(h) == x
  }
  
  // Example: Verify generator works
  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  // If you insert any two elements into an empty heap, 
  // finding the minimum of the resulting heap should get the 
  // smallest of the two elements back.
  property("return smallest") = forAll { (x: Int, y: Int) =>
  	findMin(insert(y, insert(x, empty))) == List(x,y).min  	
  }
  
  // If you insert an element into an empty heap, 
  // then delete the minimum, 
  // the resulting heap should be empty.
  property("insert to empty and delete should be empty") = forAll { x: Int =>
  	isEmpty(deleteMin(insert(x, empty)))
  }
   
  // Given any heap, you should get a sorted sequence 
  // of elements when continually finding and deleting minima. 
  // (Hint: recursion and helper functions are your friends.)
  property("acending order") = forAll { (x: Int, y: Int, z: Int) =>   
    findAndDeleteAll(insertMany(empty, List(x,y,z))) == List(x,y,z).sorted
  }
  
  // Finding a minimum of the melding of any two heaps 
  // should return a minimum of one or the other.
  property("meld two heaps") = forAll { (x: Int, y: Int) =>
    findMin(meld(insert(x,empty), insert(y, empty))) == List(x,y).min
  }

  //
  // HELPERS
  
  def insertMany(h: H, s: List[Int]): H = s match {
    case Nil => h
    case t::ts => insertMany(insert(t, h), ts)
  }
  
  def findAndDeleteAll(h: H): List[Int] = {
    def toList(l: List[Int], heap: H): List[Int] =
      if (isEmpty(heap)) l.reverse
      else toList(findMin(heap)::l, deleteMin(heap))     
    toList(Nil, h)
  }
  
}
