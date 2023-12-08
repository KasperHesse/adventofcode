package object common {
  object RangeOps {
    /**
     * Check if two ranges overlap
     * @param minX The minimum of first range
     * @param maxX The maximum of first range
     * @param minY The minimum of second range
     * @param maxY The maximum of second range
     * @return
     */
    def doOverlap(minX: Long, maxX: Long, minY: Long, maxY: Long): Boolean = {
      math.max(minX, minY) - math.min(maxX, maxY) <= 0
    }
  }

  /**
   * Class used to describe a range of numbers
   * @param start First value of the range
   * @param end Last value of the range (inclusive)
   * @param len Length of the range (end - start + 1)
   */
  case class Range(start: Long, end: Long, len: Long) {
    /**
     * Checks if this range intersects another range
     * @param that other Range
     * @return
     */
    def intersects(that: Range): Boolean = (math.max(this.start, that.start) - math.min(this.end, that.end) <= 0)

    /**
     * Partitions this range into subranges by another range.
     *
     * @param that
     * @return Three Range options, containing
     *         1: First values from this that were not covered by that
     *         2: Values from this that were covered by that
     *         3: Trailing values from this that were not covered by that
     */
    def partition(that: Range): (Option[Range], Option[Range], Option[Range]) = {
      if (!this.intersects(that)) {
        (None, None, None)
      } else {
        val (r0, r1) = if (that.start <= this.start) {
          val r0 = None
          val r1 = Some(Range(this.start, math.min(this.end, that.end), math.min(this.end, that.end) - this.start + 1))
          (r0, r1)
        } else {
          val r0 = Some(Range(this.start, that.start - 1, (that.start - 1) - this.start + 1))
          val r1 = Some(Range(that.start, math.min(this.end, that.end), math.min(this.end, that.end) - that.start + 1))
          (r0, r1)
        }
        val r2 = if (this.end <= that.end) None else Some(Range(that.end + 1, this.end, this.end - that.end + 1))
        (r0, r1, r2)
      }
    }
  }

  def leastCommonMultiple(vals: List[Long]): Long = {
    def gcd(a: Long, b: Long): Long = (a compare b) match {
      case x if x<0 => gcd(a, b-a)
      case 0 => a
      case _ => gcd(a-b, b)
    }

    vals match {
      case x :: y :: tail => {
        val a = x * (y / (gcd(x,y)))
        val b = leastCommonMultiple(tail)
        a * (b / (gcd(a,b)))
      }
      case x :: _ => x
      case Nil => 1L
    }
  }
}
