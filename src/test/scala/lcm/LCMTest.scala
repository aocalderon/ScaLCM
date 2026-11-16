package lcm

import munit.FunSuite

class LCMTest extends FunSuite {

  // Test 1: Verify that deduplicateSorted correctly drops adjacent duplicates
  test("deduplicateSorted should remove duplicates from a sorted array") {
    val sortedInput = Array(1, 1, 2, 3, 3, 3, 4, 5, 5)
    val expected = Array(1, 2, 3, 4, 5)
    
    val result = LCMOps.deduplicateSorted(sortedInput)
    
    assertEquals(result.toSeq, expected.toSeq)
  }

  // Test 2: Verify that the Transaction class correctly exposes items and weight
  test("Transaction should correctly report length and weight") {
    val items = Array(10, 20, 30)
    val transaction = new Transaction(items, weight = 5)
    
    assertEquals(transaction.length, 3)
    assertEquals(transaction.weight, 5)
    assertEquals(transaction.items.toSeq, items.toSeq)
  }

}