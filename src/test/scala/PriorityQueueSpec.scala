import common.PriorityQueue
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PriorityQueueSpec extends AnyFlatSpec with Matchers with AppendedClues {
  behavior of "PQ"

  it should "order values by increasing key" in {
    //100 iterations, generate 100 values on each iteration
    for (_ <- 0 until 100) {
      val pq = new PriorityQueue[Int](0)

      for (_ <- 0 until 100) {
        val r = scala.util.Random.nextInt()
        pq.insert(r,r)
      }
      var e = pq.extract()
      for (_ <- 0 until 99) {
        e should be <= pq.peek
        e = pq.extract()
      }
    }
  }

  it should "allow decreasing keys of items" in {
    //Insert 100 items with random prios.
    for (_ <- 0 until 100) {
      val N = 100
      val pq = new PriorityQueue[Int](0)
      val a = Array.ofDim[Int](N)
      for (item <- 0 until N) {
        val r = scala.util.Random.nextInt(100)
        pq.insert(item, r)
        a(item) = r
      }
      val decs = Seq.fill(20)(scala.util.Random.nextInt(N))
      for (d <- decs) {
        //Find
        pq.decreasePrio(d, a(d)/2)
        a(d) /= 2
      }

      //Still correct ordering
      var e1 = pq.extractWithPrio() //Second-most recent extraction
      var e2 = pq.extractWithPrio() //Most recent extraction
      for (i <- 0 until N-2) {
        e1._2 should be <= e2._2
        e1 = e2
        e2 = pq.extractWithPrio()
      }
    }
  }

  it should "allow increasing keys of items" in {

  }
}
