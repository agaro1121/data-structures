package chapter07

object BinarySearchTreeTest extends App {

  val bst: BinarySearchTreeInterface[Char] = new BinarySearchTree[Char]()

  println(bst.add('P'))
  println("****** printed *******")
  bst.iterator.foreach(print)

  println()

  println(bst.add('F'))
  println("****** printed *******")
  bst.iterator.foreach(print)

  println()

  println(bst.add('S'))
  println("****** printed *******")
  bst.iterator.foreach(print)

  println()

  println(bst.add('B'))
  println("****** printed *******")
  bst.iterator.foreach(print)

  println()

  println(bst.add('H'))
  println(bst.add('G'))
  println(bst.add('R'))
  println(bst.add('Y'))
  println(bst.add('T'))
  println(bst.add('Z'))
  println(bst.add('W'))

  println("****** printed *******")
  bst.iterator.foreach(print)

  println()

  println(bst.max)
  println(bst.min)

  println(bst.add('A'))
  println(bst.max)
  println(bst.min)
  bst.iterator.foreach(print)

  println()

  println(bst.remove('A'))
  println(bst.max)
  println(bst.min)
  bst.iterator.foreach(print)

  println()
  println()

  println(bst.getIterator(BinarySearchTreeInterface.Preorder).toList.foldLeft("")(_ + _) == "PFBHGSRYTWZ")
  println(bst.getIterator(BinarySearchTreeInterface.Inorder).toList.foldLeft("")(_ + _) == "BFGHPRSTWYZ")
  println(bst.getIterator(BinarySearchTreeInterface.Postorder).toList.foldLeft("")(_ + _) == "BGHFRWTZYSP")
}
