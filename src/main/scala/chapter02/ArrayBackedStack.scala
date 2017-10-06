package chapter02

import scala.reflect.ClassTag
import scala.util.Try

class ArrayBackedStack[T : ClassTag](val size: Int = 100)
  extends StackInterface[T] {

  private[this] val elements = new Array[T](size)
  private[this] val initialIndex = -1
  private[this] var topIndex = initialIndex

  override def push(element: T) =
    if(!isFull) {
      topIndex += 1
      elements(topIndex) = element
  } else throw new Exception

  override def pop() =
    if(!isEmpty) {
      elements(topIndex) == null
      topIndex -= 1
    } else throw new Exception


  override def top() =
    if(!isEmpty)
      elements(topIndex)
    else throw new Exception

  override def isFull = topIndex == elements.size - 1

  override def isEmpty = topIndex == initialIndex
}

object ArrayBackedStack extends App {
  val test: StackInterface[String] = new ArrayBackedStack[String]()

  test.push("trouble in the fields")
  test.push("love at the five and dime")
  test.push("once in a very blue moon")

  if (test.top().contains("once in a very blue moon"))
    println("Test 34 passed")
  else
    println("Test 34 failed")
}