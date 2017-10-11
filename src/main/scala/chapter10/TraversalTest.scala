package chapter10

object TraversalTest extends App {

  val g: WeightedGraph[String] = new ArrayWeightedGraph[String]()

  g.addVertex("austin")
  g.addVertex("houston")
  g.addVertex("atlanta")
  g.addVertex("washington")
  g.addVertex("dallas")
  g.addVertex("denver")
  g.addVertex("chicago")

  g.addEdge("austin", "dallas", 200)
  g.addEdge("austin", "houston", 160)
  g.addEdge("houston", "atlanta", 800)
  g.addEdge("dallas", "denver", 780)
  g.addEdge("dallas", "austin", 200)
  g.addEdge("dallas", "chicago", 900)
  g.addEdge("chicago", "denver", 1000)
  g.addEdge("denver", "atlanta", 1400)
  g.addEdge("denver", "chicago", 1000)
  g.addEdge("atlanta", "washington", 600)
  g.addEdge("atlanta", "houston", 800)
  g.addEdge("washington", "atlanta", 600)
  g.addEdge("washington", "dallas", 600)

  println(GraphDFS.isPath(g, "austin", "washington"))
  println()
  println(GraphBFS.isPath(g, "austin", "washington"))

}
