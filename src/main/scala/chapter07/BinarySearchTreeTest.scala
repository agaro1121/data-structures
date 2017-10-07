package chapter07

object BinarySearchTreeTest extends App {

  val bst: BinarySearchTreeInterface[Int] = new BinarySearchTree[Int]()

  println(bst.add(6))
  println("****** printed *******")
  bst.iterator.foreach(println)


}
