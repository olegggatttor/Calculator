package visitors
import tokenization.{BinaryOperation, Brace, Number}

import java.io.OutputStreamWriter

class PrintVisitor(output : OutputStreamWriter) extends TokenVisitor {
  override def visit(token: Number): Unit = {
    output.write(token.getValue)
  }

  override def visit(token: Brace): Unit = output.write(token.getValue)

  override def visit(token: BinaryOperation): Unit = output.write(token.getValue)
}
