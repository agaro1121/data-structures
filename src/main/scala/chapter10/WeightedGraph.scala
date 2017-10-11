package chapter10

import chapter04.QueueInterface

trait WeightedGraph[V] {

  def isEmpty: Boolean

  def isFull: Boolean

  def addVertex(v: V): Unit

  def hasVertex(v: V): Boolean

  def addEdge(from: V, to: V, weight: Int)

  def weightIs(from: V, to: V)

  def getToVertices(v: V): QueueInterface[V]

  def clearMarks(): Unit

  def markVertex(v: V): Unit

  def isMarked(v: V): Boolean

  def getUnmarked: V

}
