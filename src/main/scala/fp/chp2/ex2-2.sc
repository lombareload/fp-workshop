// implement is sorted with tail recursion
import scala.annotation.tailrec

def isSorted[A] (arr: Array[A], p: (A, A) => Boolean): Boolean = {
  @tailrec
  def loop(arr: Array[A]): Boolean = {
    if (arr.length <= 1) true
    else {
      val tail = arr.tail
      val sorted = p(arr.head, tail.head)
      if (sorted) loop(tail)
      else false
    }
  }
  loop(arr)
}

isSorted[Int](Array(1,2,3,5), _ <= _)
isSorted[String](Array("", "1","02", ""), _.length <= _.length)