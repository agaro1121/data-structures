package chapter02

class Balanced(openSet: String, closeSet: String) {

  /*
  * Preconditions
  * 1) lengths must be same
  * 2) Must be unique
  * */

  private var currChar: Char = _

  def test(str: String): Int = {
    require(openSet.length == closeSet.length)
    require(openSet.distinct == openSet && closeSet.distinct == closeSet)

    val stack: StackInterface[Int] = new ArrayBackedStack[Int](str.length)

    var currCharIndex = 0
    var stillBalanced: Boolean = true

    var openIndex: Int = -1
    var closeIndex: Int = -1

    while(stillBalanced && currCharIndex < str.length){
      currChar = str(currCharIndex)
      openIndex = openSet.indexOf(currChar)

      if(openIndex != -1) {
        stack.push(openIndex)
      } else {
        closeIndex = closeSet.indexOf(currChar)
        if(closeIndex != -1){
          try{
            openIndex = stack.top()
            stack.pop()
            if(closeIndex != openIndex)
              stillBalanced = false
          } catch {
            case ex: Exception =>
              stillBalanced = false
          }
        }
      }
      currCharIndex += 1
    }
    if(!stillBalanced) 1 //unbalanced symbols
    else if(!stack.isEmpty) 2 //premature end of expression
    else 0 //balanced

  }

}

object BalancedTest extends App {
  //balanced
  println(new Balanced("([{", ")]}").test("(XX(XX())XX)"))
  println(new Balanced("([{", ")]}").test("[](){}"))
  println(new Balanced("([{", ")]}").test("([]{xxx}xxx()xxx)"))
  println(new Balanced("([{", ")]}").test("([{[(([{x}])x)]}x])"))
  println(new Balanced("([{", ")]}").test("xxxxxxxxxxxxxxxxx"))

  //unbalanced
  println(new Balanced("([{", ")]}").test("(XX(XX())XXX)XXX)"))
  println(new Balanced("([{", ")]}").test("]["))
  println(new Balanced("([{", ")]}").test("( XX [ XXXX ) XX ]"))
  println(new Balanced("([{", ")]}").test("([{[(([{X}])X)]}X})"))
  println(new Balanced("([{", ")]}").test("xxxxxxxxxxxxxxxxx{"))
}