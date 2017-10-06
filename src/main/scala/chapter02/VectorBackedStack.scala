package chapter02

class VectorBackedStack[T] extends StackInterface[T] {

  private var v = Vector.empty[T]

  override def push(element: T): Unit = v = element +: v

  override def pop(): Unit = v = v.tail

  override def top(): T = v.head

  override def isFull: Boolean = false

  override def isEmpty: Boolean = v.isEmpty
}

object VectorBackedStack extends App {
  val test: StackInterface[String] = new VectorBackedStack[String]()

  test.push("trouble in the fields")
  test.push("love at the five and dime")
  test.push("once in a very blue moon")

  println(test.top())

  if (test.top().contains("once in a very blue moon"))
    println("Test 34 passed")
  else
    println("Test 34 failed")
}