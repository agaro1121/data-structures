package chapter07

import java.util.Comparator

import chapter04.{LinkedQueue, QueueInterface}

import scala.annotation.tailrec

case class BSTNode[T](info: T, left: BSTNode[T], right: BSTNode[T])

class BinarySearchTree[T](protected val comparator: Comparator[T] =
                            (o1: T, o2: T) => { o1.asInstanceOf[Comparable[T]].compareTo(o2) }
                         ) extends BinarySearchTreeInterface[T] {

  protected val root: BSTNode[T] = ???

  protected var found: Boolean = _

  override def min = {
    if(isEmpty) None
    else {
      var node = root
      while(node.left != null)
        node = node.left

      Some(node.info)
    }
  }

  override def max = {
    if(isEmpty) None
    else {
      var node = root
      while(node.right != null)
        node = node.right

      Some(node.info)
    }
  }

  private def inOrder(node: BSTNode[T], queue: QueueInterface[T]): QueueInterface[T] = {
      if(node == null)
        queue
      else {
        inOrder(node.left, queue)
        queue.enqueue(node.right.info)
        inOrder(node.right, queue)
      }
  }
  def preOrder(node: BSTNode[T], queue: QueueInterface[T]): QueueInterface[T] = {
    if(node == null)
      queue
    else {
      queue.enqueue(node.info)
      preOrder(node.left, queue)
      preOrder(node.right, queue)
    }
  }
  def postOrder(node: BSTNode[T], queue: QueueInterface[T]): QueueInterface[T] = {
    if(node == null)
      queue
    else {
      postOrder(node.left, queue)
      postOrder(node.right, queue)
      queue.enqueue(node.info)
      queue //TODO: Does this work ???
    }
  }

  override def getIterator(orderType: BinarySearchTreeInterface.Traversal) = {
    def iterate(queueInterface: QueueInterface[T]) = new Iterator[T] {
      override def hasNext = !queueInterface.isEmpty

      override def next() = queueInterface.dequeue
    }

    val queue: QueueInterface[T] = new LinkedQueue[T]

    orderType match {
      case BinarySearchTreeInterface.Inorder =>
        iterate(inOrder(root, queue))
      case BinarySearchTreeInterface.Preorder =>
        iterate(preOrder(root, queue))
      case BinarySearchTreeInterface.Postorder =>
        iterate(postOrder(root, queue))
    }
  }

  override def add(element: T) = ???

  override def get(target: T) = {
    @tailrec
    def loop(node: BSTNode[T]): Option[T] = {
      if(node == null) None
      else if(comparator.compare(target, node.info) < 0)
        loop(node.left)
      else if(comparator.compare(target, node.info) > 0)
        loop(node.right)
      else Some(node.info)
    }

    loop(root)
  }

  override def contains(target: T) = get(target).isDefined

  override def remove(target: T) = ???

  override def isFull = false

  override def isEmpty: Boolean = root == null

  override def iterator = ???
}
