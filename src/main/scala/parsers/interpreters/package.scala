package parsers

import cats.data.StateT
import cats.implicits._
import parsers.algebra.MonadState

package object interpreters {
  type Result[A] = Either[String, A]
  type P[A] = StateT[Result, String, A]

  implicit val parser: MonadState[P, String] =
    new MonadState[P, String] {
      val get: P[String] = StateT.get[Result, String]

      def modify(f: String => String): P[Unit] =
        StateT.modify[Result, String](f)
    }
}
