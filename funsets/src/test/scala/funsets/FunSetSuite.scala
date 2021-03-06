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
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet contains only its element") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton(1) 1")
      assert(!contains(s1, 2), "Singleton(1) 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all elements that are in both sets") {
    new TestSets {
      val us1 = union(s1, s2)
      val us2 = union(s2, s3)
      val i = intersect(us1, us2)
      assert(!contains(i, 1), "Intersect 1")
      assert(contains(i, 2), "Intersect 2")
      assert(!contains(i, 3), "Intersect 3")
    }
  }

  test("diff contains all elements that are in the first set but not in the second") {
    new TestSets {
      val us1 = union(s1, s2)
      val us2 = union(s2, s3)
      val d = diff(us1, us2)
      assert(contains(d, 1), "Diff 1")
      assert(!contains(d, 2), "Diff 2")
      assert(!contains(d, 3), "Diff 3")
    }
  }

  test("filter works as expected") {
    new TestSets {
      val us1 = union(s1, s2)
      val us2 = union(s2, s3)
      // contains 1,2,3
      val us3 = union(us1, us2)

      // contains only the odd numbers of the set
      val fs1 = filter(us3, (x: Int) => x % 2 != 0)
      assert(contains(fs1, 1), "Filter 1 odds")
      assert(!contains(fs1, 2), "Filter 2 odds")
      assert(contains(fs1, 3), "Filter 3 odds")

      // contains only the pair numbers of the set
      val fs2 = filter(us3, (x: Int) => x % 2 == 0)
      assert(!contains(fs2, 1), "Filter 1 pairs")
      assert(contains(fs2, 2), "Filter 2 pairs")
      assert(!contains(fs2, 3), "Filter 3 pairs")

      // contains only numbers divisible by 3
      val fs3 = filter(us3, (x: Int) => x % 3 == 0)
      assert(!contains(fs3, 1), "Filter 1 div by 3")
      assert(!contains(fs3, 2), "Filter 2 div by 3")
      assert(contains(fs3, 3), "Filter 3 div by 3")
    }
  }

  test("forall works as expected") {
    new TestSets {
      val s = union(union(s1, s2), s3)

      val c1 = (x: Int) => (x % 1 == 0)
      val c2 = (x: Int) => (x % 3 == 0)

      assert(forall(s, c1), "forall divisible by 1")
      assert(!forall(s, c2), "forall divisible by 3")
    }
  }

  test("exists works as expected") {
    new TestSets {
      val s = union(union(s1, s2), s3)

      val c1 = (x: Int) => (x % 3 == 0)
      val c2 = (x: Int) => (x == 98)

      assert(exists(s, c1), "exists divisible by 3")
      assert(!exists(s, c2), "exists 98")
    }
  }

  test("map works as expected") {
    new TestSets {
      val s = map(union(union(s1, s2), s3), (x: Int) => x * 2)

      assert(contains(s, 2), "map * 2 contains 2")
      assert(contains(s, 4), "map * 2 contains 4")
      assert(contains(s, 6), "map * 2 contains 6")
      assert(!contains(s, 1), "map * 2 not contains 1")
      assert(!contains(s, 3), "map * 2 not contains 3")
    }
  }

}
