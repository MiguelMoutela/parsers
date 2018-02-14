package parsers

import cats.implicits._
import parsers.programs.Parsers

object Main extends App {
  import interpreters._

  import Parsers._

  val p = for {
    x <- digit
    _ <- anyChar
    y <- digit
  } yield s"$x$y"


  println(string("ab").run("abc"))
}
