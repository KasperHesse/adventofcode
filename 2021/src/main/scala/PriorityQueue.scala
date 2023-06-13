import java.util.NoSuchElementException
import scala.collection.mutable

class PriorityQueue[T](N: Int) {
  require(N >= 0, "Initial size of minheap must be >=0")
  //Minheap stores items of type T
  //Each items has a key k of type int that they are sorted by
  case class Elem(item: T, var prio: Int)

  var heap: Array[Elem] = Array.ofDim[Elem](N+1)
  val elemIdx: mutable.Map[T,Int] = mutable.Map.empty[T, Int] //TODO use elemIdx everywhere
  var n: Int = N //Number of elements currently in heap

  /**
   * Look at the top item of the queue, if one such exists.
   * Throws an exception if the PQ is empty
   * @return
   */
  def peek: T = if (n == 0) throw new NoSuchElementException("Cannot peek empty PQ") else heap(1).item

  /**
   * Peek at the top item of the queue, if one such exists.
   * Returns a Some(T) if the PQ has a top item, None otherwise
   * @return
   */
  def peekOption: Option[T] = {
    try {
      Some(peek)
    } catch {
      case _: Throwable => None
    }
  }

  def insert(item: T, prio: Int): Unit = {
    n += 1
    if (n == heap.length) {
      val newHeap = Array.ofDim[Elem](n*2)
      heap.copyToArray(newHeap)
      heap = newHeap
    }
    heap(n) = Elem(item,prio)
    elemIdx(item) = n
    bubbleUp(n)
  }

  def extract(): T = {
    if (n == 0) {
      throw new NoSuchElementException("Empty PQ")
    } else {
      val r = heap(1).item
      heap(1) = heap(n)
      this.n -= 1
      bubbleDown(1)
      r
    }
  }

  /**
   * Extract an item and its priority at the same time
   * @return
   */
  def extractWithPrio(): (T, Int) = {
    if (n == 0) {
      throw new NoSuchElementException("Empty PQ")
    } else {
      val r = heap(1)
      heap(1) = heap(n)
      this.n -= 1
      bubbleDown(1)
      (r.item, r.prio)
    }
  }

  def extractOption: Option[T] = {
    try {
      Some(extract())
    } catch {
      case _: Throwable => None
    }
  }

  def decreasePrio(item: T, prio: Int): Boolean = {
    //Get index of element in elemIdx. If not set, throw noSuchElementException
    if (!elemIdx.contains(item)) {
      throw new NoSuchElementException(s"Item ${item} was not found in PQ")
    } else {
      val idx = elemIdx(item)
      if (heap(idx).prio >= prio && idx <= n) { //Can only decrease if current is greater && node is active in PQ
        heap(idx).prio = prio
        bubbleUp(idx)
        true
      } else {
        //Cannot decrease prio
        false
      }
    }
  }

  def increasePrio(item: T, prio: Int): Boolean = {
    if (!elemIdx.contains(item)) {
      throw new NoSuchElementException(s"Item $item was not found in PQ")
    } else {
      val idx = elemIdx(item)
      if (heap(idx).prio <= prio) {
        heap(idx).prio = prio
        bubbleDown(idx)
        true
      } else {
        //Cannot increase prio
        false
      }
    }
  }

  private def parent(idx: Int) = idx/2

  private def left(idx: Int) = idx*2

  private def right(idx: Int) = idx*2+1

  private def swap(p1: Int, p2: Int): Unit = {
    val t = heap(p1)
    heap(p1) = heap(p2)
    heap(p2) = t
    //Reassign elem indices
    elemIdx(heap(p2).item) = p2
    elemIdx(heap(p1).item) = p1

  }

  private def bubbleUp(idx :Int): Unit = {
    if (idx != 1) {
      val p = parent(idx)
      if (heap(p).prio > heap(idx).prio) {
        swap(p, idx)
        bubbleUp(p)
      }
    }
  }

  private def bubbleDown(idx: Int): Unit = {
    var smallest = idx
    val (l,r) = (left(idx), right(idx))
    if (l <= n && heap(l).prio < heap(smallest).prio) {
      smallest = l
    }
    if (r <= n && heap(r).prio < heap(smallest).prio) {
      smallest = r
    }
    if (smallest != idx) {
      swap(idx, smallest)
      bubbleDown(smallest)
    }
  }
  //Operations to supprt
  //Insert
  //peek (see min, don't extract)
  //extractMin / head: Remove min-value if exists
  //bubbleUp: bubble an item up to preserve heap order (used after insert)
  //bubbleDown: Bubble an item down to preserve heap order (used after extraction?)
  //decreaseKey: Reduce the key of an item: Will use bubbleUp to move the item upwards
}
