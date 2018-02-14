package parsers

import cats.data.StateT
import cats.implicits._

import scala.language.higherKinds

package object interpreters {
  type Result[A] = Either[String, A]
  type StateResult[A] = StateT[Result, String, A]

  implicit val parserInstance: Parser[StateResult] =
    new Parser[StateResult] {
      val anyChar: StateResult[Char] =
        for {
          input <- StateT.get[Result, String]
          _ <- if (input.nonEmpty) StateT.modify[Result, String](_.tail)
          else "Expected non-empty input, but input was empty.".raiseError[StateResult, Unit]
        } yield input.head
    }
}
