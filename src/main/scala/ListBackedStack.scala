import chapter02.StackInterface

class ListBackedStack[T] extends StackInterface[T]{

  private var l = List.empty[T]

  override def push(element: T): Unit = l = element :: l

  override def pop(): Unit = l = l.tail

  override def top(): T = l.head

  override def isFull: Boolean = false

  override def isEmpty: Boolean = l.isEmpty
}
