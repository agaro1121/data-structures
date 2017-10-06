package chapter04

class LinkedQueue[T] extends QueueInterface[T] {

  private var list = List.empty[T]
  private var numElements = 0
  private var front: List[T] = _
  private var rear: List[T] = _

  override def enqueue(element: T): Unit = {
    val newNode = List(element)
    if(rear == null) rear = newNode
    else front = newNode
    list = list ++ newNode
    numElements += 1
  }

  override def dequeue: T = {
    if(isEmpty) throw QueueUnderflowException
    else {
      val r = list.head
      list = list.tail
      if(isEmpty) rear = null
      numElements -= 1
      r
    }
  }

  override def isFull: Boolean = false

  override def isEmpty: Boolean = numElements == 0

  override def size: Int = numElements
}
