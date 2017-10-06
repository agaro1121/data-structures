package chapter07

trait CollectionInterface[T] {
  def add(element: T): Boolean
  def get(target: T): Option[T]
  def contains(target: T): Boolean
  def remove(target: T): Boolean
  def isFull: Boolean
  def isEmpty: Boolean
  def size: Int
}
