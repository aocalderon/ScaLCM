package lcm

import munit.FunSuite
import java.nio.file.{Files, StandardCopyOption}

class LCMIntegrationTest extends FunSuite {

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
