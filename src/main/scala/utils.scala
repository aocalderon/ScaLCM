package lcm

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.util.{Sorting, Using}

/**
 * Problem modes corresponding to LCM_FREQSET (1), LCM_CLOSED (2), LCM_MAXIMAL (4) in lcm_var.c
 */
enum LCMProblem:
  case FreqSet, Closed, Maximal

/**
 * Immutable configuration case class for the LCM miner.
 */
case class Settings(
    problem: LCMProblem,
    minSup: Int,
    outputPath: Option[String] = None,
    verbose: Boolean = false
)

/**
 * Represents a transaction (QUEUE struct in C), storing sorted frequent item IDs
 * and transaction multiplicity (`end` in Uno's QUEUE struct).
 */
final class Transaction(val items: Array[Int], var weight: Int):
  def length: Int = items.length

/**
 * Stateless helper functions (pure array & transaction operations).
 */
object LCMOps {

  def parseLineInts(line: String, out: ArrayBuffer[Int]): Unit = {
    out.clear()
    val len = line.length
    var i = 0
    while (i < len) {
      var ch = line.charAt(i)
      while (i < len && (ch < '0' || ch > '9')) {
        i += 1
        if (i < len) ch = line.charAt(i)
      }
      if (i < len && ch >= '0' && ch <= '9') {
        var num = 0
        while (i < len && ch >= '0' && ch <= '9') {
          num = num * 10 + (ch - '0')
          i += 1
          if (i < len) ch = line.charAt(i)
        }
        out += num
      }
    }
  }

  def mkFreqTrsact(
      T: Array[Transaction],
      occ: Array[Int],
      minReqItem: Int,
      maxKeptItem: Int,
      mask: Array[Boolean]
  ): Array[Transaction] = {
    val projected = new ArrayBuffer[Transaction](occ.length)
    val tempBuf   = new ArrayBuffer[Int](64)
    var i = 0
    while (i < occ.length) {
      val tr = T(occ(i))
      val items = tr.items
      if (items.nonEmpty && items(0) <= minReqItem) {
        tempBuf.clear()
        var k = 0
        while (k < items.length && items(k) <= maxKeptItem) {
          val x = items(k)
          if (mask(x)) tempBuf += x
          k += 1
        }
        if (tempBuf.nonEmpty && tempBuf(0) <= minReqItem) {
          projected += new Transaction(tempBuf.toArray, tr.weight)
        }
      }
      i += 1
    }
    projected.toArray
  }

  def occDeliver(
      T: Array[Transaction],
      maxItem: Int,
      subJump: Array[Int]
  ): Array[Array[Int]] = {
    val buckets = Array.fill(subJump.length)(new ArrayBuffer[Int]())
    val itemToPos = new Array[Int](maxItem + 1)
    java.util.Arrays.fill(itemToPos, -1)
    var i = 0
    while (i < subJump.length) {
      itemToPos(subJump(i)) = i
      i += 1
    }

    var tIdx = 0
    while (tIdx < T.length) {
      val items = T(tIdx).items
      var k = 0
      while (k < items.length && items(k) <= maxItem) {
        val pos = itemToPos(items(k))
        if (pos >= 0) buckets(pos) += tIdx
        k += 1
      }
      tIdx += 1
    }

    val result = new Array[Array[Int]](subJump.length)
    i = 0
    while (i < subJump.length) {
      result(i) = buckets(i).toArray
      i += 1
    }
    result
  }

  def shrinkTransactions(T: Array[Transaction], maxItem: Int): Array[Transaction] = {
    if (T.length <= 1) return T

    val sorted = T.clone()
    Sorting.quickSort(sorted)(using new Ordering[Transaction] {
      def compare(a: Transaction, b: Transaction): Int = {
        val ia = a.items
        val ib = b.items
        var i = 0
        while (i < ia.length && i < ib.length) {
          val va = ia(i)
          val vb = ib(i)
          if (va >= maxItem && vb >= maxItem) return 0
          if (va >= maxItem) return -1
          if (vb >= maxItem) return 1
          if (va != vb) return Integer.compare(va, vb)
          i += 1
        }
        val aHasMore = i < ia.length && ia(i) < maxItem
        val bHasMore = i < ib.length && ib(i) < maxItem
        if (aHasMore) 1 else if (bHasMore) -1 else 0
      }
    })

    val merged = new ArrayBuffer[Transaction](sorted.length)
    var currItems  = sorted(0).items
    var currWeight = sorted(0).weight

    var i = 1
    while (i < sorted.length) {
      val next = sorted(i)
      if (samePrefixBelow(currItems, next.items, maxItem)) {
        currWeight += next.weight
        currItems = intersectSuffixAbove(currItems, next.items, maxItem)
      } else {
        merged += new Transaction(currItems, currWeight)
        currItems  = next.items
        currWeight = next.weight
      }
      i += 1
    }
    merged += new Transaction(currItems, currWeight)
    merged.toArray
  }

  private def samePrefixBelow(a: Array[Int], b: Array[Int], maxItem: Int): Boolean = {
    var i = 0
    while (i < a.length && i < b.length) {
      val va = a(i)
      val vb = b(i)
      if (va >= maxItem && vb >= maxItem) return true
      if (va >= maxItem || vb >= maxItem || va != vb) return false
      i += 1
    }
    val aHasMore = i < a.length && a(i) < maxItem
    val bHasMore = i < b.length && b(i) < maxItem
    !aHasMore && !bHasMore
  }

  private def intersectSuffixAbove(a: Array[Int], b: Array[Int], maxItem: Int): Array[Int] = {
    var prefixLen = 0
    while (prefixLen < a.length && a(prefixLen) < maxItem) {
      prefixLen += 1
    }
    if (prefixLen == a.length) return a
    if (prefixLen == b.length) return java.util.Arrays.copyOf(a, prefixLen)

    val buf = new ArrayBuffer[Int](a.length)
    var k = 0
    while (k < prefixLen) {
      buf += a(k)
      k += 1
    }
    var ia = prefixLen
    var ib = prefixLen
    while (ia < a.length && ib < b.length) {
      val va = a(ia)
      val vb = b(ib)
      if (va == vb) {
        buf += va
        ia += 1
        ib += 1
      } else if (va < vb) {
        ia += 1
      } else {
        ib += 1
      }
    }
    buf.toArray
  }

  def deduplicateSorted(arr: Array[Int]): Array[Int] = {
    if (arr.length <= 1) return arr
    var uniqueCount = 1
    var i = 1
    while (i < arr.length) {
      if (arr(i) != arr(i - 1)) uniqueCount += 1
      i += 1
    }
    if (uniqueCount == arr.length) return arr
    val res = new Array[Int](uniqueCount)
    res(0) = arr(0)
    var pos = 1
    i = 1
    while (i < arr.length) {
      if (arr(i) != arr(i - 1)) {
        res(pos) = arr(i)
        pos += 1
      }
      i += 1
    }
    res
  }
}

/**
 * Base class managing buffers, database loading, frequency counting, and output I/O.
 */
abstract class LCMBase(val settings: Settings) {
  import LCMOps.*

  protected var lcmEend: Int = 0
  protected var lcmTrsactNum: Int = 0
  protected var lcmFrq: Int = 0
  protected var lcmIters: Long = 0L
  protected var fastoPerm: Array[Int] = Array.emptyIntArray

  protected var lcmOfrq: Array[Int] = Array.emptyIntArray
  protected var lcmOfrq_ : Array[Int] = Array.emptyIntArray
  protected var lcmSc: Array[Long] = Array.emptyLongArray
  protected var activeMask: Array[Boolean] = Array.emptyBooleanArray

  protected val itemsetStack   = ArrayBuffer.empty[Int]
  protected val addStack       = ArrayBuffer.empty[Int]
  protected val maximalArchive = ArrayBuffer.empty[Set[Int]]

  protected var rootTrsact: Array[Transaction] = Array.empty
  protected var rootOcc: Array[Array[Int]] = Array.empty
  protected var outWriter: Option[PrintWriter] = None

  protected def freqCalc(T: Array[Transaction], occ: Array[Int], maxItem: Int): Array[Int] = {
    val jump = new ArrayBuffer[Int](64)
    var i = 0
    while (i < occ.length) {
      val tr = T(occ(i))
      val ee = tr.weight
      val items = tr.items
      var k = 0
      while (k < items.length && items(k) <= maxItem) {
        val x = items(k)
        if (lcmOfrq(x) == 0) jump += x
        lcmOfrq(x) += ee
        k += 1
      }
      i += 1
    }
    jump.toArray
  }

  protected def loadTransactions(rawTransactionsVector: Vector[ArrayBuffer[Int]]): Int = {
    lcmTrsactNum = rawTransactionsVector.length

    // Computing item frequencies across all transactions functionally...
    val itemCounts: Map[Int, Int] = rawTransactionsVector.view
      .flatMap(identity)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap

    if (settings.verbose) {
      val maxItemSeen = if (itemCounts.nonEmpty) itemCounts.keys.maxOption.getOrElse(-1) else -1
      val rawElements = rawTransactionsVector.view.map(_.length).sum
      println(s"database: #transactions=$lcmTrsactNum, #item=${maxItemSeen + 1} #elements=$rawElements")
    }

    // Filtering frequent items (>= minSup) and sort descending by frequency, then ascending by item ID...
    val freqItems: Array[(Int, Int)] = itemCounts
      .filter(_._2 >= settings.minSup)
      .toArray
      .sortWith { case ((item1, freq1), (item2, freq2)) =>
        if (freq1 != freq2) freq1 > freq2 else item1 < item2
      }

    lcmEend = freqItems.length
    fastoPerm = new Array[Int](lcmEend)

    // Building internal mapping lookup table (original item ID -> internal sequence ID)...
    val itemToInternal = scala.collection.mutable.Map[Int, Int]()
    freqItems.zipWithIndex.foreach { case ((orig, _), internalIdx) =>
      fastoPerm(internalIdx) = orig
      itemToInternal(orig) = internalIdx
    }

    // Mapping raw buffers to internal IDs, filter infrequent items, sort, and deduplicate...
    val mappedTransactions: Array[Transaction] = rawTransactionsVector.view.flatMap { buf =>
      val mapped = buf.flatMap(itemToInternal.get)
      if (mapped.nonEmpty) {
        val sorted = mapped.toArray.sorted
        val dedup = deduplicateSorted(sorted)
        Some(new Transaction(dedup, 1))
      } else {
        None
      }
    }.toArray

    // Shrinking transactions by merging identical prefixes...
    rootTrsact = shrinkTransactions(mappedTransactions, lcmEend)

    // Returning total element sum
    rootTrsact.view.map(_.length).sum
  }  

  protected def loadDatabase(filename: String): Int = {
    val rawTransactionsVector: Vector[ArrayBuffer[Int]] = Using
      .resource(scala.io.Source.fromFile(filename)) { source =>
        source.getLines()
          .map { line =>
            val lineBuf = new ArrayBuffer[Int](64)
            LCMOps.parseLineInts(line, lineBuf)
            lineBuf
          }
          .filter(_.nonEmpty)
          .toVector
      }

    loadTransactions(rawTransactionsVector)
  }

  protected def initBuffers(): Unit = {
    lcmOfrq    = new Array[Int](lcmEend)
    lcmOfrq_   = new Array[Int](lcmEend)
    lcmSc      = new Array[Long](lcmEend + 1)
    activeMask = new Array[Boolean](lcmEend)

    settings.outputPath.foreach { path =>
      outWriter = Some(new PrintWriter(new BufferedWriter(new FileWriter(path), 65536)))
    }

    val occCounts = new Array[Int](lcmEend)
    var tIdx = 0
    while (tIdx < rootTrsact.length) {
      val tr = rootTrsact(tIdx)
      val w = tr.weight
      val items = tr.items
      var i = 0
      while (i < items.length) {
        val item = items(i)
        lcmOfrq(item) += w
        occCounts(item) += 1
        i += 1
      }
      tIdx += 1
    }

    rootOcc = new Array[Array[Int]](lcmEend)
    val occHeads = new Array[Int](lcmEend)
    var i = 0
    while (i < lcmEend) {
      rootOcc(i) = new Array[Int](occCounts(i))
      lcmOfrq_(i) = lcmOfrq(i)
      lcmOfrq(i) = 0
      i += 1
    }

    tIdx = 0
    while (tIdx < rootTrsact.length) {
      val items = rootTrsact(tIdx).items
      var k = 0
      while (k < items.length) {
        val item = items(k)
        val pos = occHeads(item)
        rootOcc(item)(pos) = tIdx
        occHeads(item) = pos + 1
        k += 1
      }
      tIdx += 1
    }
  }

  protected def addItem(origItem: Int): Unit = {
    itemsetStack += origItem
  }

  protected def trimItemsetTo(size: Int): Unit = {
    while (itemsetStack.size > size) {
      itemsetStack.remove(itemsetStack.size - 1)
    }
  }

  protected def recordSolution(): Unit = {
    val k = itemsetStack.size
    lcmSc(k) += 1
    outWriter.foreach(writeCurrentItemset)
  }

  protected def recordFreqSolution(): Unit = {
    outWriter.foreach(w => outputFreqSubsets(w, addStack.size))
    var cc = 1L
    var s = itemsetStack.size
    val t = addStack.size
    var i = 0
    while (i <= t) {
      lcmSc(s) += cc
      cc = cc * (t - i) / (i + 1)
      s += 1
      i += 1
    }
  }

  private def outputFreqSubsets(w: PrintWriter, idx: Int): Unit = {
    writeCurrentItemset(w)
    var q = idx - 1
    while (q >= 0) {
      addItem(addStack(q))
      outputFreqSubsets(w, q)
      trimItemsetTo(itemsetStack.size - 1)
      q -= 1
    }
  }

  private def writeCurrentItemset(w: PrintWriter): Unit = {
    val sb = new java.lang.StringBuilder(itemsetStack.size * 6 + 10)
    var i = 0
    while (i < itemsetStack.size) {
      sb.append(itemsetStack(i)).append(' ')
      i += 1
    }
    sb.append('(').append(lcmFrq).append(')')
    w.println(sb.toString)
  }

  protected def outputSummary(): Unit = {
    if (settings.problem != LCMProblem.Maximal && lcmSc(0) == 0) {
      lcmSc(0) = 1L
    }

    var total = 0L
    var maxK = 0
    var i = 0
    while (i <= lcmEend) {
      total += lcmSc(i)
      if (lcmSc(i) != 0) maxK = i
      i += 1
    }

    println(total)
    i = 0
    while (i <= maxK) {
      println(lcmSc(i))
      i += 1
    }

    if (settings.problem != LCMProblem.Maximal) {
      outWriter.foreach(_.println(s"($lcmTrsactNum)"))
    }

    if (settings.verbose) {
      println(s"iters=$lcmIters")
      if (settings.problem == LCMProblem.FreqSet && lcmIters > 0) {
        printf("compress ratio = %f\n", total.toDouble / lcmIters)
      }
    }
  }
}
