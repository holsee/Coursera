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
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { x: Int =>
    val h = insert(x, empty)
    findMin(h) == x
  }
  
  property("gen1") = forAll { (h: H) =>
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
    def insertMany(h: H, s: Int*) = {
      var temp = h
      for (i <- s) {
        temp = insert(i, temp)
      }
      temp
    }
  
    def deleteMany(h: H): List[Int] = {
      def toList(l: List[Int], heap: H): List[Int] =
        if (isEmpty(heap)) l.reverse
        else toList(findMin(heap)::l, deleteMin(heap))     
      toList(Nil, h)
    }
  	
    deleteMany(insertMany(empty, x, y, z)) == List(x,y,z).sorted
  }

}
