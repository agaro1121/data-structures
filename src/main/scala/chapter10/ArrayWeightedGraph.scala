package chapter10
import chapter04.{ArrayBoundedQueue, LinkedQueue, QueueInterface}

import scala.reflect.ClassTag

class ArrayWeightedGraph[V : ClassTag](val size: Int = 50) extends WeightedGraph[V] {

  private val NullEdge = 0
  private var numVertices: Int = 0
  private val vertices: Array[V] = Array.ofDim[V](size)
  private val edges: Array[Array[Int]] = Array.ofDim[Int](size, size)
  private val marks: Array[Boolean] = Array.ofDim[Boolean](size)

  override def isEmpty: Boolean = numVertices == 0

  override def isFull: Boolean = numVertices == size

  override def addVertex(v: V): Unit = {
    vertices.update(numVertices, v)
    for(index <- 0 until numVertices){
      edges(numVertices).update(index, NullEdge)
      edges(index).update(numVertices, NullEdge)
    }
    numVertices += 1
  }

  override def hasVertex(v: V): Boolean = vertices.exists(_ == v)

  override def addEdge(from: V, to: V, weight: Int): Unit = {
    val row = vertices.indexOf(from)
    val column = vertices.indexOf(to)
    edges(row).update(column, weight)
  }

  override def weightIs(from: V, to: V): Unit = {
    val row = vertices.indexOf(from)
    val column = vertices.indexOf(to)
    edges(row)(column)
  }

  override def getToVertices(v: V): QueueInterface[V] = {
    val adjVertices: QueueInterface[V] = new ArrayBoundedQueue[V]()
    val fromIndex = vertices.indexOf(v)

  for{
      toIndex <- 0 until numVertices
      if edges(fromIndex)(toIndex) != NullEdge
     }{
        val nextVertex = vertices(toIndex)
        adjVertices.enqueue(nextVertex)
      }

    adjVertices
  }

  override def clearMarks(): Unit = for(i <- 0 to numVertices) marks.update(i, false)

  override def markVertex(v: V): Unit = marks.update(vertices.indexOf(v), true)

  override def isMarked(v: V): Boolean = marks(vertices.indexOf(v))

  override def getUnmarked: V = vertices(marks.indexWhere(_ == false)) //FIXME: might throw exception. Use while loop
}
