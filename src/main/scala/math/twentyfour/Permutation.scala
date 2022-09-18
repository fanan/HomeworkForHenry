package math.twentyfour

import scala.math.Ordered.orderingToOrdered

class Permutation[A <: Comparable[A]](inputs: Array[A]) extends Iterator[Array[A]] {
  private[this] val buffer: Array[A] = inputs.sorted

  private[this] var _hasNext = buffer.nonEmpty
  private[this] val n = buffer.length


  override def hasNext: Boolean =
    _hasNext

  override def next(): Array[A] = {
    val ret = buffer.clone()
    if (buffer.length < 2) {
      _hasNext = false
    } else {
      val indexLeftOpt = (n - 1).until(0, -1).find(i => {
        buffer(i) > buffer(i - 1)
      })
      if (indexLeftOpt.isEmpty) {
        _hasNext = false
      } else {
        val indexLeft = indexLeftOpt.get - 1
        var targetIndex = indexLeftOpt.get
        var currentMin = buffer(targetIndex)
        (targetIndex + 1).until(n).foreach(i => {
          if (buffer(i) > buffer(indexLeft) && currentMin > buffer(i)) {
            targetIndex = i
            currentMin = buffer(i)
          }
        })
        // swap
        swap(targetIndex, indexLeft)
        // reverse
        reverse(indexLeft + 1)
        _hasNext = true
      }
    }
    ret
  }

  private[this] def reverse(first: Int): Unit = {
    val last = n - 1
    val counts = last - first + 1
    0.until(counts / 2).foreach(i => {
      swap(first + i, last - i)
    })
  }

  private[this] def swap(i: Int, j: Int): Unit = {
    val tmp = buffer(i)
    buffer(i) = buffer(j)
    buffer(j) = tmp
  }
}
