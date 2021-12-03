import tokenization.{End, Start, TokenState}
import visitors.{CalcVisitor, ParserVisitor, PrintVisitor}

import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

object Calculator {
  def main(args: Array[String]): Unit = {
    val expression = readLine()

    val buffer = ArrayBuffer.empty[TokenState]
    buffer += Start(expression, 0)
    while (!buffer.last.isInstanceOf[End]) {
      buffer.last.tokenize(buffer)
    }

    val parser = new ParserVisitor()
    for (token <- buffer) {
      token.accept(parser)
    }

    val writer = new OutputStreamWriter(System.out)
    val printer = new PrintVisitor(writer)
    for (token <- parser.getResult) {
      token.accept(printer)
    }
    writer.write("\n")
    writer.flush()

    val calculator = new CalcVisitor()
    for (token <- parser.getResult) {
      token.accept(calculator)
    }
    println(s"Result of expression: ${calculator.getResult}.")
  }
}
