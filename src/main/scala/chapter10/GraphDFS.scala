package chapter10

import chapter02.{ArrayBackedStack, StackInterface, VectorBackedStack}
import chapter04.{LinkedQueue, QueueInterface}

import scala.reflect.ClassTag

object GraphDFS {

  def isPath[V : ClassTag](graph: WeightedGraph[V], start: V, end: V): Boolean = {

    /*
    Set found to false
    Clear all marks
    Mark the startVertex
    Push the startVertex onto the stack
    do
      Set current vertex = stack.top()
      stack.pop()
      if current vertex equals endVertex
         Set found to true
      else
      for each adjacent vertex
         if adjacent vertex is not marked
            Mark the adjacent vertex and
            Push it onto the stack
    while !stack.isEmpty() AND !found
    return found
    * */

    var found: Boolean = false

    val stack: StackInterface[V] = new ArrayBackedStack[V]
    var vertexQueue: QueueInterface[V] = new LinkedQueue[V]

    graph.clearMarks()
    graph.markVertex(start)
    stack.push(start)

    do{
      val currentVertex: V = stack.top()
      stack.pop()
      println(currentVertex)
      if(currentVertex == end)
        found = true
      else {
       vertexQueue = graph.getToVertices(currentVertex)
        while(!vertexQueue.isEmpty){
          val adjVertex = vertexQueue.dequeue
          if(!graph.isMarked(adjVertex)){
            graph.markVertex(adjVertex)
            stack.push(adjVertex)
          }
        }
      }

    } while(!stack.isEmpty && !found)

    found
  }

}
