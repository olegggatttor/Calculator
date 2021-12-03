package visitors
import tokenization.{BinaryOperation, Brace, Div, LeftBrace, Mul, Number, RightBrace, Sub, Sum, TokenState}

import java.text.ParseException
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ParserVisitor extends TokenVisitor {
  private val opStack = new mutable.Stack[TokenState]
  private val result = ArrayBuffer.empty[TokenState]
  private val precedence = (token : TokenState) => token match {
    case Sum(_, _) => 1
    case Sub(_, _) => 1
    case Mul(_, _) => 2
    case Div(_, _) => 2
    case LeftBrace(_, _) => -1
    case RightBrace(_, _) => -1
  }
  override def visit(token: Number): Unit = {
    result += token
  }
  override def visit(token: Brace): Unit = {
    token match {
      case LeftBrace(_, _) => opStack.push(token)
      case RightBrace(_, _) =>
        while (!opStack.top.isInstanceOf[LeftBrace]) {
          result += opStack.pop()
          if (opStack.isEmpty) {
            throw new ParseException("Cannot find matching left brace for right brace.", token.getPos)
          }
        }
        opStack.pop()
    }
  }
  override def visit(token: BinaryOperation): Unit = {
    while (opStack.nonEmpty && precedence(token)<= precedence(opStack.top)) {
      result += opStack.pop()
    }
    opStack.push(token)
  }

  def getResult: Seq[TokenState] = {
    result.addAll(opStack)
    opStack.clear()
    result.toSeq
  }
}
