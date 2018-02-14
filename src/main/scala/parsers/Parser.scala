package parsers

import scala.language.higherKinds

trait Parser[F[_]] {
  val anyChar: F[Char]
}

object Parser {
  def apply[F[_]](implicit P: Parser[F]): Parser[F] = P
}
