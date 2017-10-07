package chapter07

import java.util.Comparator

import chapter04.{LinkedQueue, QueueInterface}

import scala.annotation.tailrec

case class BSTNode[T](info: T, left: BSTNode[T] = null, right: BSTNode[T] = null)

class BinarySearchTree[T](protected val comparator: Comparator[T] = new Comparator[T] {
  override def compare(o1: T, o2: T): Int = {
    o1.asInstanceOf[Comparable[T]].compareTo(o2)
  }}) extends BinarySearchTreeInterface[T] {

  protected var root: BSTNode[T] = _

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

  override def add(element: T) = {

    def loop(node: BSTNode[T]): BSTNode[T] = {
      if(node == null)
        BSTNode(element, null, null)
      else if(comparator.compare(element, node.info) <= 0)
        node.copy(left = loop(node.left))
      else node.copy(right = loop(node.right))
    }


    root = loop(root)
    true
  }

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

  override def remove(target: T) = {
    root = recRemove(target, root)
    root != null
  }

  private def recRemove(target: T, node: BSTNode[T]): BSTNode[T] = {
    if(node == null) null
    else if(comparator.compare(target, node.info) <= 0)
      node.copy(left = recRemove(target, node.left))
    else if(comparator.compare(target, node.info) > 0)
      node.copy(right = recRemove(target, node.right))
    else {
      removeNode(node)
    }
  }

  private def removeNode(node: BSTNode[T]): BSTNode[T] = {
    if(node.left == null) node.right
    else if(node.right == null) node.left
    else {
      val t = getPredecessor(node.left)
      node.copy(info = t, left = recRemove(t, node.left))
    }
  }

  private def getPredecessor(node: BSTNode[T]): T = {
    var temp = node

    while(temp.right != null)
      temp = temp.right

    temp.info
  }

  override def isFull = false

  override def isEmpty: Boolean = root == null

  override def iterator = getIterator(orderType = BinarySearchTreeInterface.Inorder)
}
