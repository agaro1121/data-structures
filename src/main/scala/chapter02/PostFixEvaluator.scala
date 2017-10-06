package chapter02

import scala.util.Try

class Scanner(str: String){
  private val splitStr: Array[String] = str.split("\\ ")
  private var currIndex = 0

  def nextInt(): Int = {
    val r = splitStr(currIndex).toInt
    currIndex += 1
    r
  }

  def next(): String = {
    val s = splitStr(currIndex)
    currIndex += 1
    s
  }

  def hasNext(): Boolean = {
    currIndex <= splitStr.length - 1
  }

  def hasNextInt(): Boolean = {
    (currIndex <= splitStr.length - 1) &&
    Try(splitStr(currIndex).toInt).isSuccess
  }


}

trait PostFixException
case class TooManyOperands(msg: String) extends PostFixException
case object NotEnoughOperands extends PostFixException
case object IllegalSymbol extends PostFixException

class PostFixEvaluator {

  var value: Int = _
  var operator: String = _
  var operand1: Int = _
  var operand2: Int = _
  var result: Int = _


  def evaluate(expr: String): Either[PostFixException, Int] = {
    require(expr.nonEmpty)

    val stack: StackInterface[Int] = new ArrayBackedStack[Int](expr.size)

    val scanner = new Scanner(expr)

    while(scanner.hasNext()){

      if(scanner.hasNextInt()) {
        value = scanner.nextInt()

        if (stack.isFull)
          return Left(TooManyOperands("Too many operands-stack overflow"))

        stack.push(value)
      } else {

        operator = scanner.next()

        if(!isOperator(operator))
          return Left(IllegalSymbol)

        if(stack.isEmpty)
          return Left(NotEnoughOperands)

        operand2 = stack.top()
        stack.pop()

        if(stack.isEmpty)
          return Left(NotEnoughOperands)

        operand1 = stack.top()
        stack.pop()

        result = performOperation(operand1, operand2, operator)
        stack.push(result)
      }

    }

    // Obtain final result from stack.
    if (stack.isEmpty)
       Left(NotEnoughOperands)
    result = stack.top();
    stack.pop();
    // Stack should now be empty.
    if (!stack.isEmpty)
      Left(TooManyOperands("Too many operands-operands left over"))
    // Return the final.
    Right(result)
  }

  private def isOperator(o: String): Boolean = o == "/" || o == "*" || o == "+" || o == "-"

  private def performOperation(op1: Int, op2: Int, op: String): Int = {
    op match {
      case "/" => op1 / op2
      case "+" => op1 + op2
      case "-" => op1 - op2
      case "*" => op1 * op2
    }
  }

}

object PostFixEvaluator extends App {
  println(new PostFixEvaluator().evaluate("5 7 + 6 2 - *"))
  println(new PostFixEvaluator().evaluate("4 2 3 5 1 - + * + *"))
}
