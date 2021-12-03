package visitors

import tokenization.{BinaryOperation, Brace, Number}

import scala.collection.mutable

class CalcVisitor extends TokenVisitor {
  private val evalStack = new mutable.Stack[Int]()

  override def visit(token: Number): Unit = {
    evalStack.push(token.getValue.trim.toInt)
  }

  override def visit(token: Brace): Unit = {}

  override def visit(token: BinaryOperation): Unit = evalStack.push(
    token.eval(evalStack.pop(), evalStack.pop())
  )

  def getResult: Int = evalStack.top
}
