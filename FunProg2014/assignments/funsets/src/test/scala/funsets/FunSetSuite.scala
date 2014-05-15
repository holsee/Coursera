package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }

  
  import FunSets._


  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s0 = singletonSet(0)
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(1000)
    var all = union(union(union(union(s0, s1), s2), s3), s4)
    var evens = filter(all, e => e % 2 == 0)
    var odds = filter(all, e => e % 2 != 0)
  }

  test("contains returns true when found") {
    assert(contains(Set(100), 100))
  }

  test("contains returns false when not found") {
    assert(!contains(Set(101), 100))
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      assert(FunSets.toString(union(s0, s4)) === "{0,1000}" )
    }
  }

  test("intersect contains shared elements") {
    new TestSets {
      val s = intersect(s1, s1)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("no intersection means no elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains 'complement' of first set") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter odds") {
    new TestSets {
      val s = filter(all, e => e % 2 != 0)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(contains(s, 3), "Diff 3")
    }
  }

  test("filter evens") {
    new TestSets {
      val s = filter(all, e => e % 2 == 0)
      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(evens, e => e % 2 == 0)) 
      assert(!forall(evens, e => e % 2 != 0))
      assert(forall(odds, e => e % 2 != 0)) 
      assert(!forall(odds, e => e % 2 == 0)) 
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s1, e => e == 1)) 
      assert(exists(s2, e => e % 2 == 0))
      assert(exists(all, e => e % 2 == 0))
      assert(!exists(odds, e => e % 2 == 0))
    }
  }

  test("map") {
    new TestSets {
      val s = map(s1, e => e + 1)
      assert(contains(s, 2))
    }
  }

  test("map -1") {
    new TestSets {
      assert( FunSets.toString(map(all, e => e - 1)) === "{-1,0,1,2,999}" )
    }
  }
}
