package tokenization

import exceptions.TokenizerException
import visitors.TokenVisitor

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

sealed abstract class TokenState(input: String, inputPos: Int) {
  protected val LeftBracePattern: Regex = "\\(".r
  protected val RightBracePattern: Regex = "\\)".r
  protected val NumberPattern: Regex = "\\d".r
  protected val SumPattern: Regex = "\\+".r
  protected val SubPattern: Regex = "-".r
  protected val UnaryMinusPattern: Regex = "-".r
  protected val MulPattern: Regex = "\\*".r
  protected val DivPattern: Regex = "/".r

  def tokenize(buffer: ArrayBuffer[TokenState]): Unit

  def accept(visitor: TokenVisitor): Unit

  def getValue: String

  def getPos: Int = inputPos


  protected def skipWs(startPos: Int): Int = {
    var curPos = startPos
    while (curPos < input.length && input(curPos).isWhitespace) {
      curPos += 1
    }
    curPos
  }

  protected def expectLeftBraceOrNumber(buffer: ArrayBuffer[TokenState]): Unit = {
    val curPos = skipWs(inputPos)
    if (curPos < input.length) {
      val curToken = input(curPos).toString
      curToken match {
        case LeftBracePattern() =>
          buffer += LeftBrace(input, curPos + 1)
        case NumberPattern() =>
          buffer += Number(input, curPos)
        case _ => throw new TokenizerException(s"Expected number or left brace, found: $curToken")
      }
    } else {
      throw new TokenizerException(s"Unexpected end of input after '${input(curPos - 1)}'.")
    }
  }
}

abstract class Brace(input: String, inputPos: Int) extends TokenState(input, inputPos) {
  override def accept(visitor: TokenVisitor): Unit = visitor.visit(this)
}

abstract class BinaryOperation(input: String, inputPos: Int) extends TokenState(input, inputPos) {
  override def accept(visitor: TokenVisitor): Unit = visitor.visit(this)

  def eval(left : Int, right : Int) : Int
}

final case class Start(input: String, inputPos: Int) extends TokenState(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def accept(visitor: TokenVisitor): Unit = {}

  override def getValue: String = ""
}

final case class End(input: String, inputPos: Int) extends TokenState(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = {}

  override def accept(visitor: TokenVisitor): Unit = {}

  override def getValue: String = ""
}

final case class LeftBrace(input: String, inputPos: Int) extends Brace(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def getValue: String = "("
}

final case class RightBrace(input: String, inputPos: Int) extends Brace(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = {
    val curPos = skipWs(inputPos)
    if (curPos < input.length) {
      val curToken = input(curPos).toString
      curToken match {
        case RightBracePattern() =>
          buffer += RightBrace(input, curPos + 1)
        case SumPattern() =>
          buffer += Sum(input, curPos + 1)
        case SubPattern() =>
          buffer += Sub(input, curPos + 1)
        case MulPattern() =>
          buffer += Mul(input, curPos + 1)
        case DivPattern() =>
          buffer += Div(input, curPos + 1)
        case _ => throw new TokenizerException(s"Expected number or left brace, found: $curToken")
      }
    } else {
      buffer += End(input, curPos)
    }
  }

  override def getValue: String = ")"
}

final case class Sum(input: String, inputPos: Int) extends BinaryOperation(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def getValue: String = " + "

  override def eval(left: Int, right: Int): Int = left + right
}

final case class Sub(input: String, inputPos: Int) extends BinaryOperation(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def getValue: String = " - "

  override def eval(left: Int, right: Int): Int = right - left
}

final case class Mul(input: String, inputPos: Int) extends BinaryOperation(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def getValue: String = " * "

  override def eval(left: Int, right: Int): Int = left * right
}

final case class Div(input: String, inputPos: Int) extends BinaryOperation(input, inputPos) {
  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = expectLeftBraceOrNumber(buffer)

  override def getValue: String = " / "

  override def eval(left: Int, right: Int): Int = right / left
}

final case class Number(input: String, inputPos: Int) extends TokenState(input, inputPos) {
  private var value: Option[Int] = None

  def getValue: String = s" ${value.get.toString} "

  override def tokenize(buffer: ArrayBuffer[TokenState]): Unit = {
    val skipPos = skipWs(inputPos)
    var numberPos = skipPos
    val numberBuffer = new StringBuilder()
    while (numberPos < input.length && input(numberPos).isDigit) {
      numberBuffer.append(input(numberPos))
      numberPos += 1
    }
    value = Some(numberBuffer.toString.toInt)
    val curPos = skipWs(numberPos)
    if (curPos < input.length) {
      val curToken = input(curPos).toString
      curToken match {
        case SumPattern() =>
          buffer += Sum(input, curPos + 1)
        case SubPattern() =>
          buffer += Sub(input, curPos + 1)
        case MulPattern() =>
          buffer += Mul(input, curPos + 1)
        case DivPattern() =>
          buffer += Div(input, curPos + 1)
        case RightBracePattern() =>
          buffer += RightBrace(input, curPos + 1)
        case _ => throw new TokenizerException(s"Expected operation, found: $curToken")
      }
    } else {
      buffer += End(input, curPos)
    }
  }

  override def accept(visitor: TokenVisitor): Unit = visitor.visit(this)
}