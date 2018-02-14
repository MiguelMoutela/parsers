package parsers

import cats.implicits._
import Arithmetic._
import interpreters._

object Main extends App {
  val expression = "((1)) * (2 + (((3))) * (4 + (((5)) + 6)) * (((7 * 8))) + 9)"

  val result = expr.run(expression)

  println(result)
}
