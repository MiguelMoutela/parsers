package parsers

import cats._
import cats.implicits._
import language.higherKinds

object Arithmetic {

  import Parsers._

  def expr[F[_]: Parser: ParserError: SemigroupK]: F[Int] =
    for {
      t <- term
      res <- (for {
        _ <- symbol("+")
        e <- expr
      } yield t + e) <+> t.pure[F]
    } yield res

  def term[F[_]: Parser: ParserError: SemigroupK]: F[Int] =
    for {
      f <- factor
      res <- (for {
        _ <- symbol("*")
        t <- term
      } yield f * t) <+> f.pure[F]
    } yield res

  def factor[F[_]: Parser: ParserError: SemigroupK]: F[Int] =
    (for {
      _ <- symbol("(")
      e <- expr
      _ <- symbol(")")
    } yield e) <+> natural
}
