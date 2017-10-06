package chapter03

import scala.annotation.tailrec

object BinarySearch {

  def apply[T](elem: T, arr: Array[T])(implicit o: Ordering[T]): Int = {

    @tailrec
    def loop(from: Int, to: Int): Int = {
      if (from > to) return -1

      val middleIndex = (from + to) / 2

      arr(middleIndex) match {
        case `elem` => middleIndex

        case n if o.gt(elem, n) =>
          loop(middleIndex + 1, arr.length - 1)

        case n if o.lt(elem, n) =>
          loop(0, middleIndex - 1)

        case _ => -1
      }
    }

    loop(0, arr.length)
  }

}

object BinarySearchTest extends App {

  val arr = Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  val arr2 = Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)

  println(BinarySearch(5, arr))
  println(BinarySearch(25, arr))
  println(BinarySearch(5, arr2))
  println(BinarySearch(20, arr2))

}