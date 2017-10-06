package chapter02

trait StackInterface[T] {

  @throws[Exception]
  def push(element: T): Unit

  @throws[Exception]
  def pop(): Unit

  def top(): T

  def isFull: Boolean

  def isEmpty: Boolean
}

