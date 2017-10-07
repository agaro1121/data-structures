package chapter07

import java.util.Comparator
import chapter04.{LinkedQueue, QueueInterface}

case class BSTNode[T](info: T, left: Option[BSTNode[T]] = None, right: Option[BSTNode[T]] = None)

class BinarySearchTree[T](protected val comparator: Comparator[T] = new Comparator[T] {
  override def compare(o1: T, o2: T): Int = {
    o1.asInstanceOf[Comparable[T]].compareTo(o2)
  }}) extends BinarySearchTreeInterface[T] {

  protected var root: Option[BSTNode[T]] = None

  protected var found: Boolean = _

  override def min = {
    if(isEmpty) None
    else {
      var node = root
      while(node.get.left.isDefined)
        node = node.flatMap(_.left)

      node.map(_.info)
    }
  }

  override def max = {
    if(isEmpty) None
    else {
      var node = root
      while(node.get.right.isDefined)
        node = node.flatMap(_.right)

      node.map(_.info)
    }
  }

  private def inOrder(node: Option[BSTNode[T]], queue: QueueInterface[T]): QueueInterface[T] = {
    node match {
      case None => queue
      case Some(n) =>
        inOrder(n.left, queue)
        queue.enqueue(n.info)
        inOrder(n.right, queue)
    }
  }

  def preOrder(node: Option[BSTNode[T]], queue: QueueInterface[T]): QueueInterface[T] = {
    node match {
      case None => queue
      case Some(n) =>
        queue.enqueue(n.info)
        preOrder(n.left, queue)
        preOrder(n.right, queue)
    }
  }

  def postOrder(node: Option[BSTNode[T]], queue: QueueInterface[T]): QueueInterface[T] = node match {
    case None => queue
    case Some(n) =>
      postOrder(n.left, queue)
      postOrder(n.right, queue)
      queue.enqueue(n.info)
      queue //TODO: Does this work ???

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

  override def add(element: T) = {

    def loop(node: Option[BSTNode[T]]): BSTNode[T] = node match {
      case None => BSTNode(element)
      case Some(n) if comparator.compare(element, n.info) <= 0 =>
        n.copy(left = Some(loop(n.left)))
      case Some(n) =>  n.copy(right = Some(loop(n.right)))
    }


    val n  = loop(root)
    root = Some(n)
    true
  }

  override def get(target: T) = {

    def loop(node: Option[BSTNode[T]]): Option[T] = {
      node.flatMap { n =>
        if (comparator.compare(target, n.info) < 0)
          loop(n.left)
        else if (comparator.compare(target, n.info) > 0)
          loop(n.right)
        else Some(n.info)
      }
    }

    loop(root)
  }

  override def contains(target: T) = get(target).isDefined

  override def remove(target: T) = {
    root = recRemove(target, root)
    root.isDefined
  }

  private def recRemove(target: T, node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
    node.flatMap{ n =>
      if(comparator.compare(target, n.info) < 0)
        Some(n.copy(left = recRemove(target, node.flatMap(_.left))))
      else if(comparator.compare(target, n.info) > 0)
        Some(n.copy(right = recRemove(target, node.flatMap(_.right))))
      else {
        removeNode(node)
      }
    }

  }

  private def removeNode(node: Option[BSTNode[T]]): Option[BSTNode[T]] = {
    node.flatMap{ n =>
      if(n.left.isEmpty) n.right
      else if(n.right.isEmpty) n.left
      else {
        val tt = getPredecessor(n.left)
        tt.map{ t =>
          n.copy(info = t, left = recRemove(t, n.left))
        }
      }
    }
  }

  private def getPredecessor(node: Option[BSTNode[T]]): Option[T] = {
    var temp = node

    val right = temp.flatMap(_.right)

    while(right.isDefined)
      temp = right

    temp.map(_.info)
  }

  override def isFull = false

  override def isEmpty: Boolean = root.isEmpty

  override def iterator = getIterator(orderType = BinarySearchTreeInterface.Inorder)
}
