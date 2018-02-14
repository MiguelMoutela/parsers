package parsers.programs

import cats._
import cats.implicits._
import parsers.algebra.MonadState

import scala.language.higherKinds

object Parsers {

  def anyChar[F[_]](implicit F0: MonadState[F, String],
                    F1: MonadError[F, String]): F[Char] =
    for {
      input <- F0.get
      _ <- if (input.nonEmpty) F0.modify(_.tail)
      else F1.raiseError[Char]("Expected non-empty input, but input was empty.")
    } yield input.head

  def sat[F[_]](p: Char => Boolean, name: String = "Char => Boolean")(
      implicit F0: MonadState[F, String],
      F1: MonadError[F, String]): F[Char] =
    for {
      c <- anyChar
      res <- if (p(c)) c.pure[F]
      else F1.raiseError[Char](s"Expected $name, but found '$c'.")
    } yield res

  def digit[F[_]](implicit F0: MonadState[F, String],
                  F1: MonadError[F, String]): F[Char] = sat(_.isDigit, "digit")

  def char[F[_]](c: Char)(implicit F0: MonadState[F, String],
                          F1: MonadError[F, String]): F[Char] =
    sat(_ == c, s"'$c'")

  def string[F[_]](s: String)(implicit F0: MonadState[F, String],
                              F1: MonadError[F, String],
                              F2: ApplicativeError[F, String]): F[String] =
    if (s.isEmpty) F1.pure("")
    else F2.map2(char(s.head), string(s.tail))(_ + (_: String))

  def many[F[_], A](p: F[A])(implicit F0: MonadState[F, String],
                             F1: MonadError[F, String],
                             F2: Applicative[F]): F[List[A]] =
    many1(p) <+> F1.pure(List.empty[A])

  def many1[F[_], A](p: F[A])(implicit F0: MonadState[F, String],
                              F1: MonadError[F, String],
                              F2: ApplicativeError[F, String]): F[List[A]] =
    F2.map2(p, many(p))(_ :: _)

  def nat[F[_]](implicit F0: MonadState[F, String],
                F1: MonadError[F, String],
                F2: Alternative[F]): F[Int] =
    F1.map(many1(digit))(_.mkString.toInt)

  def space[F[_]](implicit F0: MonadState[F, String],
                  F1: MonadError[F, String],
                  F2: Alternative[F]): F[Unit] =
    F1.map(many(sat(_.isWhitespace)))(_ => ())

  def token[F[_], A](p: F[A])(implicit F0: MonadState[F, String],
                              F1: MonadError[F, String],
                              F2: Alternative[F]): F[A] =
    F1.flatMap(space)(_ => p)

  def natural[F[_]](implicit F0: MonadState[F, String],
                    F1: MonadError[F, String],
                    F2: Alternative[F]): F[Int] = token(nat)

  def symbol[F[_]](s: String)(implicit F0: MonadState[F, String],
                              F1: MonadError[F, String],
                              F2: Alternative[F]): F[String] = token(string(s))
}
