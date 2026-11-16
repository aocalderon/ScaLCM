package lcm

import scala.collection.mutable.ArrayBuffer
import LCMOps.*
import mainargs.{main, arg, Flag}

object LCM {

  @main
  def run(
      @arg(name = "mode", doc = "Mining mode: C (Closed), F (Frequent), or M (Maximal)")
      modeStr: String,
      
      @arg(name = "input", doc = "Path to the input transactional dataset file")
      inputFile: String,
      
      @arg(name = "support", doc = "Minimum support threshold count")
      support: Int,
      
      @arg(name = "output", doc = "Optional path to write output itemsets")
      outputFile: Option[String] = None,

      @arg(name = "verbose", doc = "Enable verbose logging")
      verbose: Flag = Flag()
  ): Unit = {
    val problem = modeStr.toUpperCase match {
      case "C" | "CLOSED"  => LCMProblem.Closed
      case "F" | "FREQ"    => LCMProblem.FreqSet
      case "M" | "MAXIMAL" => LCMProblem.Maximal
      case other           => throw new IllegalArgumentException(s"Unknown mode: $other. Use C, F, or M.")
    }

    val settings = Settings(
      problem = problem,
      minSup = support,
      outputPath = outputFile,
      verbose = verbose.value
    )

    val miner = new LCMMiner(settings)
    miner.run(inputFile)
  }

  // Automatically wires up the mainargs parser to the @main annotation above
  def main(args: Array[String]): Unit = mainargs.ParserForMethods(this).runOrExit(args)
}

class LCMMiner(settings: Settings) extends LCMBase(settings) {

  def run(inputFile: String): Unit = {
    val totalElements = loadDatabase(inputFile)

    if (rootTrsact.isEmpty || lcmEend == 0) {
      if (settings.verbose) println("there is no frequent itemset")
      outputSummary()
      return
    }

    if (settings.verbose) {
      println(s"shrinked database: #transactions=${rootTrsact.length}, #item=$lcmEend #elements=$totalElements")
    }

    initBuffers()

    try {
      settings.problem match {
        case LCMProblem.Closed  => runClosed()
        case LCMProblem.FreqSet => runFreq()
        case LCMProblem.Maximal => runMaximal()
      }
      outputSummary()
    } finally {
      outWriter.foreach(_.close())
    }
  }

  def run(transactions: Vector[Array[Int]]): Unit = {
    val totalElements = loadTransactions(transactions)
    if (rootTrsact.isEmpty || lcmEend == 0) {
      if (settings.verbose) println("there is no frequent itemset")
      outputSummary()
      return
    }

    if (settings.verbose) {
      println(s"shrinked database: #transactions=${rootTrsact.length}, #item=$lcmEend, #elements=$totalElements")
    }

    initBuffers()

    try {
      settings.problem match {
        case LCMProblem.Closed  => runClosed()
        case LCMProblem.FreqSet => runFreq()
        case LCMProblem.Maximal => runMaximal()
      }
      outputSummary()
    } finally {
      outWriter.foreach(_.close())
    }
  }

  private def runClosed(): Unit = {
    val rootClosure = new ArrayBuffer[Int]()
    val candidates  = new ArrayBuffer[Int]()
    var i = 0
    while (i < lcmEend) {
      if (lcmOfrq_(i) == lcmTrsactNum) rootClosure += i
      else candidates += i
      i += 1
    }

    if (rootClosure.nonEmpty) {
      rootClosure.foreach(e => addItem(fastoPerm(e)))
      lcmFrq = lcmTrsactNum
      recordSolution()
    }

    var cIdx = 0
    while (cIdx < candidates.length) {
      val item = candidates(cIdx)
      lcmClosedIter(rootTrsact, rootOcc(item), item, lcmOfrq_(item))
      lcmOfrq_(item) = 0
      lcmOfrq(item)  = 0
      cIdx += 1
    }
    lcmIters += 1
  }

  private def lcmClosedIter(
      T: Array[Transaction],
      occ: Array[Int],
      item: Int,
      frq: Int
  ): Int = {
    lcmIters += 1
    val savedStackSize = itemsetStack.size
    addItem(fastoPerm(item))
    lcmFrq = frq

    val jump = freqCalc(T, occ, lcmEend - 1)
    lcmOfrq(item) = 0

    var flag = -1
    val survivingJumpBuf = new ArrayBuffer[Int](jump.length)
    val subJumpBuf       = new ArrayBuffer[Int](jump.length)

    var i = 0
    while (i < jump.length) {
      val x = jump(i)
      val f = lcmOfrq(x)
      if (f >= settings.minSup) {
        if (f < lcmFrq) {
          survivingJumpBuf += x
          if (x < item) subJumpBuf += x
        } else {
          if (x < item) addItem(fastoPerm(x))
          else if (x > flag) flag = x
        }
      }
      i += 1
    }

    if (flag > item) {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
      trimItemsetTo(savedStackSize)
      return flag
    }

    recordSolution()

    if (subJumpBuf.isEmpty) {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
      trimItemsetTo(savedStackSize)
      return -1
    }

    val subJump = subJumpBuf.toArray
    java.util.Arrays.sort(subJump)

    val subFreqs = new Array[Int](subJump.length)
    i = 0
    while (i < subJump.length) {
      subFreqs(i) = lcmOfrq(subJump(i))
      i += 1
    }

    i = 0
    while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }

    val maxSubItem = subJump(subJump.length - 1)
    var maxKeptItem = maxSubItem
    i = 0
    while (i < survivingJumpBuf.length) {
      val x = survivingJumpBuf(i)
      activeMask(x) = true
      if (x > maxKeptItem) maxKeptItem = x
      i += 1
    }

    val rawTT = mkFreqTrsact(T, occ, maxSubItem, maxKeptItem, activeMask)

    i = 0
    while (i < survivingJumpBuf.length) {
      activeMask(survivingJumpBuf(i)) = false
      i += 1
    }

    val TT = if (subJump.length >= 2 && rawTT.length > 5) shrinkTransactions(rawTT, item) else rawTT
    val subOcc = occDeliver(TT, maxSubItem, subJump)

    var idx = subJump.length - 1
    while (idx >= 0) {
      lcmClosedIter(TT, subOcc(idx), subJump(idx), subFreqs(idx))
      idx -= 1
    }

    trimItemsetTo(savedStackSize)
    -1
  }

  private def runFreq(): Unit = {
    var i = 0
    while (i < lcmEend) {
      lcmFreqIter(rootTrsact, rootOcc(i), i, lcmOfrq_(i))
      lcmOfrq_(i) = 0
      lcmOfrq(i)  = 0
      i += 1
    }
    lcmIters += 1
  }

  private def lcmFreqIter(
      T: Array[Transaction],
      occ: Array[Int],
      item: Int,
      frq: Int
  ): Unit = {
    lcmIters += 1
    val savedItemsetSize = itemsetStack.size
    val savedAddSize     = addStack.size

    addItem(fastoPerm(item))
    lcmFrq = frq

    val jump = if (item > 0) freqCalc(T, occ, item - 1) else Array.emptyIntArray
    val surviving = new ArrayBuffer[Int](jump.length)

    var i = 0
    while (i < jump.length) {
      val x = jump(i)
      val f = lcmOfrq(x)
      if (f >= settings.minSup) {
        if (f == lcmFrq) addStack += fastoPerm(x)
        else surviving += x
      }
      i += 1
    }

    recordFreqSolution()

    if (surviving.nonEmpty) {
      val subJump = surviving.toArray
      java.util.Arrays.sort(subJump)

      val subFreqs = new Array[Int](subJump.length)
      i = 0
      while (i < subJump.length) {
        subFreqs(i) = lcmOfrq(subJump(i))
        activeMask(subJump(i)) = true
        i += 1
      }

      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }

      val maxSubItem = subJump(subJump.length - 1)
      val rawTT = mkFreqTrsact(T, occ, maxSubItem, maxSubItem, activeMask)

      i = 0
      while (i < subJump.length) {
        activeMask(subJump(i)) = false
        i += 1
      }

      val TT = if (subJump.length >= 3 && rawTT.length >= 20) shrinkTransactions(rawTT, lcmEend) else rawTT
      val subOcc = occDeliver(TT, maxSubItem, subJump)

      var idx = subJump.length - 1
      while (idx >= 0) {
        lcmFreqIter(TT, subOcc(idx), subJump(idx), subFreqs(idx))
        idx -= 1
      }
    } else {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
    }

    trimItemsetTo(savedItemsetSize)
    while (addStack.size > savedAddSize) addStack.remove(addStack.size - 1)
  }

  private def runMaximal(): Unit = {
    var i = 0
    while (i < lcmEend) {
      lcmMaxIter(rootTrsact, rootOcc(i), i, lcmOfrq_(i))
      lcmOfrq_(i) = 0
      lcmOfrq(i)  = 0
      i += 1
    }
    lcmIters += 1
  }

  private def lcmMaxIter(
      T: Array[Transaction],
      occ: Array[Int],
      item: Int,
      frq: Int
  ): Int = {
    lcmIters += 1
    val savedStackSize = itemsetStack.size
    addItem(fastoPerm(item))
    lcmFrq = frq

    val jump = freqCalc(T, occ, lcmEend - 1)
    lcmOfrq(item) = 0

    var flag = -1
    val survivingJumpBuf = new ArrayBuffer[Int](jump.length)
    val subJumpBuf       = new ArrayBuffer[Int](jump.length)

    var i = 0
    while (i < jump.length) {
      val x = jump(i)
      val f = lcmOfrq(x)
      if (f >= settings.minSup) {
        if (f < lcmFrq) {
          survivingJumpBuf += x
          if (x < item) subJumpBuf += x
        } else {
          if (x < item) addItem(fastoPerm(x))
          else if (x > flag) flag = x
        }
      }
      i += 1
    }

    if (flag > item) {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
      trimItemsetTo(savedStackSize)
      return flag
    }

    if (survivingJumpBuf.isEmpty) {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
      val candidateSet = itemsetStack.toSet
      if (!maximalArchive.exists(sup => candidateSet.subsetOf(sup))) {
        maximalArchive += candidateSet
        recordSolution()
      }
      trimItemsetTo(savedStackSize)
      return -1
    }

    if (subJumpBuf.nonEmpty) {
      val subJump = subJumpBuf.toArray
      java.util.Arrays.sort(subJump)

      val subFreqs = new Array[Int](subJump.length)
      i = 0
      while (i < subJump.length) {
        subFreqs(i) = lcmOfrq(subJump(i))
        i += 1
      }

      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }

      val maxSubItem = subJump(subJump.length - 1)
      var maxKeptItem = maxSubItem
      i = 0
      while (i < survivingJumpBuf.length) {
        val x = survivingJumpBuf(i)
        activeMask(x) = true
        if (x > maxKeptItem) maxKeptItem = x
        i += 1
      }

      val rawTT = mkFreqTrsact(T, occ, maxSubItem, maxKeptItem, activeMask)

      i = 0
      while (i < survivingJumpBuf.length) {
        activeMask(survivingJumpBuf(i)) = false
        i += 1
      }

      val TT = if (subJump.length >= 2 && rawTT.length > 7) shrinkTransactions(rawTT, item) else rawTT
      val subOcc = occDeliver(TT, maxSubItem, subJump)

      var idx = subJump.length - 1
      while (idx >= 0) {
        lcmMaxIter(TT, subOcc(idx), subJump(idx), subFreqs(idx))
        idx -= 1
      }
    } else {
      i = 0
      while (i < jump.length) { lcmOfrq(jump(i)) = 0; i += 1 }
    }

    trimItemsetTo(savedStackSize)
    -1
  }
}
