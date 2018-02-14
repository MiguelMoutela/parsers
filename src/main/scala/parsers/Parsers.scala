package parsers

import cats._
import cats.implicits._

import scala.language.higherKinds

object Parsers {

  type ParserError[F[_]] = MonadError[F, String]

  def anyChar[F[_]: Parser]: F[Char] = Parser[F].anyChar

  def sat[F[_]: Parser: ParserError](
      p: Char => Boolean,
      name: String = "Char => Boolean"): F[Char] =
    for {
      c <- anyChar
      res <- if (p(c)) c.pure[F]
      else
        MonadError[F, String]
          .raiseError[Char](s"Expected $name, but found '$c'.")
    } yield res

  def digit[F[_]: Parser: ParserError]: F[Char] = sat(_.isDigit, "digit")

  def char[F[_]: Parser: ParserError](c: Char): F[Char] =
    sat(_ == c, s"'$c'")

  def string[F[_]: Parser: ParserError: SemigroupK](s: String): F[String] =
    if (s.isEmpty) "".pure[F]
    else (char(s.head), string(s.tail)).mapN(_ + _)

  def many[F[_]: Parser: ParserError: SemigroupK, A](p: F[A]): F[List[A]] =
    many1(p) <+> List.empty[A].pure[F]

  def many1[F[_]: Parser: ParserError: SemigroupK, A](p: F[A]): F[List[A]] =
    for {
      v  <- p
      vs <- many(p)
    } yield v :: vs

  def nat[F[_]: Parser: ParserError: SemigroupK]: F[Int] =
    many1(digit).map(_.mkString.toInt)

  def space[F[_]: Parser: ParserError: SemigroupK]: F[Unit] =
    many(sat(_.isWhitespace, "whitespace")).map(_ => ())

  def token[F[_]: Parser: ParserError: SemigroupK, A](p: F[A]): F[A] =
    for {
      _ <- space
      t <- p
      _ <- space
    } yield t

  def natural[F[_]: Parser: ParserError: SemigroupK]: F[Int] = token(nat)

  def symbol[F[_]: Parser: ParserError: SemigroupK](s: String): F[String] =
    token(string(s))
}
