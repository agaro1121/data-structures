package chapter07

trait BinarySearchTreeInterface[T]
  extends CollectionInterface[T]
  with Iterable[T] {

  def min: Option[T]

  def max: Option[T]

  def getIterator(orderType: BinarySearchTreeInterface.Traversal): Iterator[T]

}

object BinarySearchTreeInterface {
  sealed trait Traversal
  case object Inorder extends Traversal
  case object Preorder extends Traversal
  case object Postorder extends Traversal
}