import cats.data.StateT._
import cats.data.StateT
import cats.implicits._

package object parsers {

  type Parser[A] = StateT[Option, String, A]

  val item: Parser[Char] =
    for {
      input <- get[Option, String]
      _ <- if (input.nonEmpty) modify[Option, String](_.tail)
      else ().raiseError[Parser, Nothing]
    } yield input.head

  def sat(p: Char => Boolean): Parser[Char] =
    for {
      c <- item
      _ <- if (p(c)) c.pure[Parser]
      else ().raiseError[Parser, Nothing]
    } yield c

  val digit: Parser[Char] = sat(_.isDigit)

  val lower: Parser[Char] = sat(_.isLower)

  val upper: Parser[Char] = sat(_.isUpper)

  val letter: Parser[Char] = sat(_.isLetter)

  val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)

  def char(c: Char): Parser[Char] = sat(_ == c)

  def string(str: String): Parser[String] =
    str.map(char).toList.sequence.map(_.mkString)

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) <+> List.empty[A].pure[Parser]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      head <- p
      tail <- many(p)
    } yield head :: tail

  val ident: Parser[String] =
    (lower, many(alphaNum)).mapN(_ :: _).map(_.mkString)

  val nat: Parser[Int] = many1(digit).map(_.mkString.toInt)

  val space: Parser[Unit] = many(sat(_.isWhitespace)).map(_ => ())

  def token[A](p: Parser[A]): Parser[A] =
    space *> p <* space

  val identifier: Parser[String] = token(ident)
  val natural: Parser[Int] = token(nat)
  def symbol(s: String): Parser[String] = token(string(s))
}
