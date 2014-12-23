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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val oneTwo = (x: Int) => x == 1 || x == 2
    val empty = (x: Int) => false
    val full = (x: Int) => true
    val odd = (x: Int) => (x + 1) % 2 == 0

    def multOfX(x: Int) = (y: Int) => y % x == 0
    val even = multOfX(2)
    val multOfTen = multOfX(10)
    val multOfThree = multOfX(3)

    val neg = (x: Int) => x < 0
    val pos = (x: Int) => x > 0
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
      assert(contains(s1, 1), "Singleton 1")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
      assert(!contains(s, 0), "Union 0")

      assert(contains(union(empty, full), 100), "union empty with full")
    }
  }

  test("intersection") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 0), "Intersect 0")
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
      assert(!contains(s, 4), "Intersect 4")

      val two = intersect(s2, oneTwo);
      assert(contains(two, 2), "s2 and oneTwo contain 2")
      assert(!contains(two, 1), "s2 and oneTwo don't contain 1")

      assert(!contains(intersect(empty, full), 100), "intersect empty with full")
    }
  }

  test("diff") {
    new TestSets {
      val diffS1S2 = diff(s1, s2)
      assert(contains(diffS1S2, 1), "diff s1 with s2 contains 1")
      assert(!contains(diffS1S2, 2), "diff s1 with s2 doesn't contain 2")

      val diffOneTwoS1 = diff(oneTwo, s1)
      assert(contains(diffOneTwoS1, 2), "diff oneTwo with s1 contains 2")
      assert(!contains(diffOneTwoS1, 1), "diff oneTwo with s1 doesn't contain 1")

      val emptyFull = diff(empty, full)
      assert(!contains(emptyFull, 100), "emptyFull is empty")

      val fullEmpty = diff(full, empty)
      assert(contains(fullEmpty, 100), "fullEmpty is full")
    }
  }

  test("filter") {
    new TestSets {
      val s1s2 = filter(s1, s2)
      assert(!contains(s1s2, 2), "s1s2 does not contain 2")
      assert(!contains(s1s2, 1), "s1s2 does not contain 1")

      val two = filter(s2, oneTwo);
      assert(contains(two, 2), "s2 and oneTwo contain 2")
      assert(!contains(two, 1), "s2 and oneTwo don't contain 1")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(multOfTen, even), "all mults of 10 are even")
      assert(!forall(even, multOfTen), "not evens are mults of 10")
      assert(!forall(even, odd), "whatever")
      assert(forall(odd, (x: Int) => !even(x)), "all odds must not be even")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(even, multOfTen), "there exists an even mult of 10")
      assert(exists(multOfTen, even), "there exists a mult of ten that's even")
      assert(!exists(even, odd), "there are no even odds")
      assert(!exists(odd, even), "there are no odd evens")
      assert(!exists(odd, multOfTen), "there are no odd mults of 10")
      assert(exists(multOfThree, multOfTen))
      assert(exists(multOfThree, multOfX(4)))
      assert(!exists(neg, pos));
      assert(!exists(pos, neg));
    }
  }

  test("map") {
    new TestSets {
      val mapOdd = map(even, (x: Int) => x + 1)
      assert(!exists(even, mapOdd), "map evens to odds");
      assert(exists(odd, mapOdd), "map evens to odds");

      val onePlusOne = map(s1, (x: Int) => x + 1)
      assert(contains(onePlusOne, 2), "one plus one contains two")
      assert(!contains(onePlusOne, 1), "one plus one doesn't contain one")

      val mapToZero = map(odd, (x: Int) => 0);
      assert(contains(mapToZero, 0));
      assert(!contains(mapToZero, 1));
    }
  }
}
