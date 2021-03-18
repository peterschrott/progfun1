package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
    val s1: FunSet = singletonSet(1)
    val s2: FunSet = singletonSet(2)
    val s3: FunSet = singletonSet(3)
    val s123: FunSet = union(union(s1, s2), s3)

    def createSet(elements: Int*): FunSet = {
      elements.headOption.map { head =>
        val init = singletonSet(head)
        elements.tail.foldLeft(init)((set, element) => union(set, singletonSet(element)))
      } getOrElse emptySet
    }
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   */
  @Test def `singleton set one contains one`: Unit = {

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

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains only elements that are in either set`: Unit = {
    new TestSets {
      val sU1 = union(s1, s2)
      val sU2 = union(s1, s3)
      val sI = intersect(sU1, sU2)

      assert(contains(sI, 1), "Intersect 1")
      assert(!contains(sI, 2), "Intersect 2")
      assert(!contains(sI, 3), "Intersect 3")
    }
  }

  @Test def `diff contains only elements that are not in other set`: Unit = {
    new TestSets {
      val sU1 = union(s1, s2)
      val sU2 = union(s1, s3)
      val sD = diff(sU1, sU2)

      assert(!contains(sD, 1), "Diff 1")
      assert(contains(sD, 2), "Diff 2")
      assert(!contains(sD, 3), "Diff 3")

      val set1 = createSet(1, 3, 4, 5, 7, 1000)
      val set2 = createSet(1, 2, 3, 4)
      val sD2 = diff(set1, set2)
      val expected = Array(5, 7, 1000)
      expected.foreach(e => {
        assert(contains(sD2, e), s"diff should contain $e")
      })
      val notExpected = Array(1, 2, 3, 4)
      notExpected.foreach(e => {
        assert(!contains(sD2, e), s"diff should NOT contain $e")
      })
    }
  }

  @Test def `filter contains only elements where predicate applies`: Unit = {
    new TestSets {
      val sF = filter(s123, e => e != 2)

      assert(contains(sF, 1), "Filter 1")
      assert(!contains(sF, 2), "Filter 2")
      assert(contains(sF, 3), "Filter 3")
    }
  }

  @Test def `forall must return true if all elements in the given set satisfy the predicate, otherwise false`: Unit = {
    new TestSets {
      assert(forall(s123, e => e > 0), "Forall 1")
      assert(forall(s123, e => e < 4), "Forall 2")
      assert(forall(s123, e => e != 9), "Forall 3")
      assert(!forall(s123, e => e != 1), "Forall 4")
      assert(!forall(s123, e => e == 1), "Forall 5")
    }
  }

  @Test def `exists must return true if at least one elements in the given set satisfy the predicate, otherwise false`: Unit = {
    new TestSets {
      assert(!exists(s123, e => e < 0), "Exists 1")
      assert(!exists(s123, e => e > 4), "Exists 2")
      assert(exists(s123, e => e > 0), "Exists 3")
      assert(exists(s123, e => e < 4), "Exists 4")
      assert(exists(s123, e => e != 9), "Exists 5")
      assert(exists(s123, e => e != 1), "Exists 6")
      assert(exists(s123, e => e == 1), "Exists 7")
    }
  }

  @Test def `map contains only elements where predicate applies`: Unit = {
    new TestSets {
      val sM = map(s123, e => e * 2)

      assert(contains(sM, 2), "Map 1")
      assert(contains(sM, 4), "Map 2")
      assert(contains(sM, 6), "Map 3")
      assert(!contains(sM, 1), "Map 4")
      assert(!contains(sM, 3), "Map 5")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
