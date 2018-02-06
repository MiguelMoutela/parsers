package parsers

import cats.implicits._

object ArithmeticExpressions {

  /*
   grammer:

   expr ::= term (+ expr | \epsilon) \
   term ::= factor (* term | \epsilon) \
   factor ::= (expr) | nat \
   nat ::= 0 | 1 | 2 | \dots \
   */

  lazy val expr: Parser[Int] =
    for {
      t <- term
      res <- (for {
        _ <- symbol("+")
        e <- expr
      } yield t + e) <+> t.pure[Parser]
    } yield res

  lazy val term: Parser[Int] =
    for {
      f <- factor
      res <- (for {
        _ <- symbol("*")
        t <- term
      } yield f * t) <+> f.pure[Parser]
    } yield res

  lazy val factor: Parser[Int] =
    (for {
      _ <- symbol("(")
      e <- expr
      _ <- symbol(")")
    } yield e) <+> natural

  def eval(input: String): Either[String, Int] =
    expr.run(input) match {
      case Some(("", n))  => Right(n)
      case Some((out, _)) => Left(s"unconsumed input: $out")
      case None           => Left("invalid input")
    }
}
