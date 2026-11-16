package lcm

import munit.FunSuite
import java.nio.file.{Files, StandardCopyOption}
import scala.collection.mutable.ArrayBuffer

class LCMIntegrationTest extends FunSuite {
  test("loadTransactions should correctly parse dataset, apply minSup filter, and build internal transaction structures") {
    val outputPath = "/tmp/test.txt"
    val settings = Settings(
      problem = LCMProblem.Closed,
      minSup = 2,
      outputPath = Some(outputPath),
      verbose = true
    )

    // Instantiate the miner for Closed itemsets with support = 2
    val lcm = new LCMMiner(settings)

    // Loading dummy transactions...
    val transactions: Vector[Array[Int]] = Vector(
      Array(1, 2, 3, 4),
        Array(1, 2, 4),
        Array(2, 3, 4, 5),
        Array(2, 4, 5),   
        Array(1, 3, 5)
    )
    lcm.run(transactions)

    val results: ArrayBuffer[Itemset] = lcm.results
    val frequents = results.map{ itemset =>
      (itemset.items.sorted.mkString(" "), itemset.support)
    }.sortBy(-_._2).map{ case(items, support) =>
        s"$items ($support)"
    }.mkString("\n")
    println(frequents)

    // Assertions
    val outPathObj = java.nio.file.Paths.get(outputPath)
    assert(Files.exists(outPathObj), "Output file should be created by the miner")
      
    val fileLines = Files.readAllLines(outPathObj)
    assert(!fileLines.isEmpty, "Output file should contain discovered itemsets")
      
    assert(fileLines.size() == 10, "Output file finds the correct number of itemsets")
  }

  test("LCMMiner runs successfully on T10I4D100K dataset with min_support = 200") {
    val resourceStream = getClass.getClassLoader.getResourceAsStream("T10I4D100K.dat")
    
    if (resourceStream == null) {
      fail("T10I4D100K.dat not found in src/main/resources")
    } else {
      // Copy the resource stream to a temporary file on disk so BufferedReader/FileReader can read it
      val tempFile = Files.createTempFile("T10I4D100K", ".dat")
      Files.copy(resourceStream, tempFile, StandardCopyOption.REPLACE_EXISTING)
      resourceStream.close()

      val inputPath = tempFile.toAbsolutePath.toString
      val outputPath = "/tmp/lcm_test_output.txt"

      // Clean up previous output if it exists
      Files.deleteIfExists(java.nio.file.Paths.get(outputPath))

      // Instantiate using the new Settings case class
      val settings = Settings(
        problem = LCMProblem.Closed,
        minSup = 200,
        outputPath = Some(outputPath),
        verbose = true
      )

      // Instantiate the miner for Closed itemsets with support = 200
      val miner = new LCMMiner(settings)

      // Execute the mining run
      miner.run(inputPath)

      // Assertions
      val outPathObj = java.nio.file.Paths.get(outputPath)
      assert(Files.exists(outPathObj), "Output file should be created by the miner")
      
      val fileLines = Files.readAllLines(outPathObj)
      assert(!fileLines.isEmpty, "Output file should contain discovered itemsets")
      
      assert(fileLines.size() == 13108, "Output file finds the correct number of itemsets")
    }
  }
}
