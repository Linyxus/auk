package auk.tui.app

/** The column-width distribution for a table — the pure integer-allocation core
  * of width-aware table layout, kept separate from [[Layout]] so it can be tested
  * without a terminal.
  *
  * It is the CSS 2.1 §17.5.2 "automatic table layout" rule:
  *   - if the columns' natural widths fit, each keeps its natural width;
  *   - otherwise space is shared, shrinking each column from its natural toward
  *     its minimum in proportion to how much slack it has;
  *   - if even the minimums don't fit, the minimums themselves are scaled down
  *     (cells will then character-break).
  *
  * Rounding uses the largest-remainder method, so the returned widths sum exactly
  * to `avail` (when `avail` can hold one column per cell).
  */
object TableLayout:

  /** Column widths summing to `avail` (when feasible). `natural(c)` is the column's
    * unwrapped content width, `min(c)` its minimum sensible width; both ≥ 1. */
  def columnWidths(natural: Vector[Int], min: Vector[Int], avail: Int): Vector[Int] =
    val n = natural.length
    if n == 0 then Vector.empty
    else
      val sumNat = natural.sum
      val sumMin = min.sum
      if sumNat <= avail then natural
      else if sumMin <= avail then
        // Distribute the surplus over the minimums in proportion to each column's
        // slack (natural − min).
        val surplus = avail - sumMin
        val flex = natural.lazyZip(min).map((nat, mn) => (nat - mn).toDouble)
        val shares = largestRemainder(flex, surplus)
        min.lazyZip(shares).map(_ + _)
      else
        // Even the minimums overflow: scale them to the available width.
        largestRemainder(min.map(_.toDouble), avail).map(math.max(_, 1))

  /** Distribute `total` integer units across buckets in proportion to `weights`,
    * giving the leftover units to the largest fractional parts. */
  private def largestRemainder(weights: Vector[Double], total: Int): Vector[Int] =
    val n = weights.length
    if n == 0 || total <= 0 then Vector.fill(n)(0)
    else
      val sumW = weights.sum
      val effective = if sumW <= 0 then Vector.fill(n)(1.0) else weights
      val effSum = effective.sum
      val exact = effective.map(w => total * w / effSum)
      val floors = exact.map(math.floor(_).toInt)
      var remaining = total - floors.sum
      // Indices ordered by descending fractional part (stable by index on ties).
      val order = exact.indices.sortBy(i => (-(exact(i) - math.floor(exact(i))), i))
      val result = floors.toArray
      var k = 0
      while remaining > 0 && k < order.length do
        result(order(k)) += 1
        remaining -= 1
        k += 1
      result.toVector
