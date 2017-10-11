package chapter10

import chapter04.{LinkedQueue, QueueInterface}

object GraphBFS {

  def isPath[V](graph: WeightedGraph[V], start: V, end: V): Boolean = {

    var found: Boolean = false

    val stack: QueueInterface[V] = new LinkedQueue[V]
    var vertexQueue: QueueInterface[V] = new LinkedQueue[V]

    graph.clearMarks()
    graph.markVertex(start)
    stack.enqueue(start)

    do{
      val currentVertex: V = stack.dequeue
      println(currentVertex)
      if(currentVertex == end)
        found = true
      else {
        vertexQueue = graph.getToVertices(currentVertex)
        while(!vertexQueue.isEmpty){
          val adjVertex = vertexQueue.dequeue
          if(!graph.isMarked(adjVertex)){
            graph.markVertex(adjVertex)
            stack.enqueue(adjVertex)
          }
        }
      }

    } while(!stack.isEmpty && !found)

    found
  }


}
