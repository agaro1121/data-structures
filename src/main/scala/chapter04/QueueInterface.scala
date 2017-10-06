package chapter04

import scala.reflect.ClassTag

case object QueueOverflowException extends Exception
case object QueueUnderflowException extends Exception

trait QueueInterface[T] {

  @throws[QueueOverflowException.type]
  def enqueue(element: T): Unit

  @throws[QueueUnderflowException.type]
  def dequeue: T

  def isFull: Boolean

  def isEmpty: Boolean

  def size: Int

}

class ArrayBoundedQueue[T : ClassTag](val capacity: Int = 100) extends QueueInterface[T] {

  private val elements: Array[T] = new Array[T](capacity)
  private var frontIndex: Int = 0
  private var rearIndex: Int = capacity - 1
  private var numElements: Int = 0

  override def enqueue(element: T): Unit = {
    if(isFull) throw QueueOverflowException
    else {
      rearIndex = (rearIndex + 1) % elements.length
      elements.update(rearIndex, element)
      numElements += 1
    }
  }

  override def dequeue: T = {
    if(isEmpty) throw QueueUnderflowException
    else {
      val r = elements(frontIndex)
      frontIndex = (frontIndex + 1) % elements.length
      numElements -= 1
      r
    }
  }

  override def isFull: Boolean = numElements == capacity

  override def isEmpty: Boolean = numElements == 0

  override def size: Int = numElements
}

object QueueInterfaceTester extends App {
  val list = List("the beginning of a story", "is often different than", "the end of a story")

//  val queue: QueueInterface[String] = new ArrayBoundedQueue[String](3)
  val queue: QueueInterface[String] = new LinkedQueue[String]

  list.foreach(queue.enqueue)

  println("Order is:\n")
  while(!queue.isEmpty) {
    println(queue.dequeue)
  }

}