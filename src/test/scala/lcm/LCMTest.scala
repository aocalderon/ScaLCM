package lcm

import munit.FunSuite

class LCMTest extends FunSuite {
  test("deduplicateSorted should remove duplicates from a sorted array") {
    val sortedInput = Array(1, 1, 2, 3, 3, 3, 4, 5, 5)
    val expected = Array(1, 2, 3, 4, 5)
    
    val result = LCMOps.deduplicateSorted(sortedInput)
    
    assertEquals(result.toSeq, expected.toSeq)
  }

  test("Transaction should correctly report length and weight") {
    val items = Array(10, 20, 30)
    val transaction = new Transaction(items, weight = 5)
    
    assertEquals(transaction.length, 3)
    assertEquals(transaction.weight, 5)
    assertEquals(transaction.items.toSeq, items.toSeq)
  }

  test("parseInts should correctly extract numbers from a string line") {
    val line = "12 34 5"
    val result = LCMOps.parseInts(line)
    
    assertEquals(result.toSeq, Seq(12, 34, 5))
  }
}
