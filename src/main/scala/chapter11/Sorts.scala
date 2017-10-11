package chapter11

import scala.annotation.tailrec
import Ordering.Implicits._
import scala.util.Random

object Sorts {

  def isSorted[T : Ordering](array: Array[T]): Boolean = array.sliding(2).forall(arr => arr(0) <= arr(1))

  private def swap[T](array: Array[T], currentIndex: Int, minIndex: Int): Array[T] = {
    val minValue = array(minIndex)
    array.update(minIndex, array(currentIndex))
    array.update(currentIndex, minValue)
    array
  }

  def selectionSort[T : Ordering](items: Array[T]): Array[T] = {

    def minIndex(subArray: Array[T], start: Int, end: Int): Int = {
      @tailrec
      def loop(subArray: Array[T], start: Int, end: Int, current: Int): Int = {
        if(start >= end) current
        else {
          val nextIndex = start + 1
          loop(subArray, start + 1, end, if(subArray(nextIndex) < subArray(current)) nextIndex else current)
        }
      }
      loop(subArray, start, end, start)
    }

    if(items.size < 1) items
    else {
      val endIndex = items.size - 1

      for(i <- 0 to endIndex){
        val min = minIndex(items, i, endIndex)
        swap(items, i, min)
      }

      items
    }
    
  }

}

object SortsTester extends App {
  val array = new Array[Int](10)
  val rand = new Random()

  for(i <- 0 to 9)(array(i) = Math.abs(rand.nextInt()) % 100)

  private val selectionSorted: Array[Int] = Sorts.selectionSort(array)
  println(selectionSorted.mkString(", "))
  println(Sorts.isSorted(array))
}