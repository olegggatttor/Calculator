package visitors

import tokenization._
trait TokenVisitor {
  def visit(token : Number) : Unit
  def visit(token : Brace) : Unit
  def visit(token : BinaryOperation) : Unit
}
