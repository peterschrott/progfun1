package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  // Part 1

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a', 'b', 'd'), chars(t2))
    }

  @Test def `make code tree`: Unit =
    new TestTrees {
      val l = Leaf('a', 2)
      val r = Leaf('b', 3)
      assertEquals(t1, makeCodeTree(l, r))
    }

  // Part 2

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `count the frequency of chars in a list`: Unit = {
    val chars = List('a', 'b', 'a', 'c', 'b')
    assertEquals(List('a' -> 2, 'b' -> 2, 'c' -> 1), times(chars))
  }

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leafList))
  }

  @Test def `singleton must return true on 1 element list otherwise false`: Unit = {
    val l1 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val l2 = List()
    val l3 = List(Leaf('e', 1))

    assertEquals(false, singleton(l1))
    assertEquals(false, singleton(l2))
    assertEquals(true, singleton(l3))
  }

  @Test def `combine first 2 elements of list to a fork`: Unit = {
    val leafList = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leafList))
  }

  @Test def `create tree of chars`: Unit =  {
    val chars = List('a', 'b', 'a', 'c', 'b')
    val expResult = Fork(Leaf('a', 2), Fork(Leaf('c', 1), Leaf('b', 2), List('c', 'b'), 3), List('a', 'c', 'b'), 5)

    val result = createCodeTree(chars)

    assertEquals(expResult, result)
  }

  // Part 3

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
