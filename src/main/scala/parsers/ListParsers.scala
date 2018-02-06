package parsers

import cats.data.NonEmptyList
import cats.implicits._

object ListParsers {
  val commaFollowedByNatural: Parser[Int] =
    for {
      _ <- symbol(",")
      n <- natural
    } yield n

  val nonEmptyIntList: Parser[NonEmptyList[Int]] =
    for {
      _ <- symbol("[")
      n <- natural
      ns <- many(commaFollowedByNatural)
      _ <- symbol("]")
    } yield NonEmptyList(n, ns)
}
