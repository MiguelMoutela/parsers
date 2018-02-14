package parsers.algebra

import language.higherKinds

trait MonadState[F[_], State] {
  val get: F[State]
  def modify(f: State => State): F[Unit]
}
