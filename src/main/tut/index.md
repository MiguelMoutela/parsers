# Sample code for blog post

```tut:book
type Parser[A] = String => Option[(A, String)]
```

```tut:silent
import cats.data.StateT._
import cats.data.StateT
import cats.implicits._
```

```tut:book
type Parser[A] = StateT[Option, String, A]
```

```tut:book
"42".pure[Parser]

().raiseError[Parser, Nothing]
```

```tut:silent
val item: Parser[Char] =
  for {
    input <- get[Option, String]
    _ <- if (input.nonEmpty) 
      modify[Option, String](_.tail)
    else 
      ().raiseError[Parser, Nothing]
  } yield input.head
```

```tut:silent
def sat(p: Char => Boolean): Parser[Char] =
  for {
    c <- item
    _ <- if (p(c)) c.pure[Parser]
    else ().raiseError[Parser, Nothing]
  } yield c
```

```tut:silent
val digit: Parser[Char] = sat(_.isDigit)

val lower: Parser[Char] = sat(_.isLower)

val upper: Parser[Char] = sat(_.isUpper)

val letter: Parser[Char] = sat(_.isLetter)

val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)

def char(c: Char): Parser[Char] = sat(_ == c)
```

```tut:silent
def string(str: String): Parser[String] = 
  str.map(char).toList.sequence.map(_.mkString)
```

```tut:book
val p = string("hi") <+> string("hello")

p.run("hello world")
```

```tut:silent
object Many { // needed for tut
  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) <+> List.empty[A].pure[Parser]
  
  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      head <- p
      tail <- many(p)
    } yield head :: tail
}

import Many._
```

```tut:silent
val ident: Parser[String] =
  (lower, many(alphaNum)).mapN(_ :: _).map(_.mkString)

val nat: Parser[Int] =
  many1(digit).map(_.mkString.toInt)

val space: Parser[Unit] =
  many(sat(_.isWhitespace)).map(_ => ())

def token[A](p: Parser[A]): Parser[A] =
  space *> p <* space

val identifier: Parser[String] = token(ident)
val natural: Parser[Int] = token(nat)
def symbol(s: String): Parser[String] = token(string(s))
``` 

[latex]
expr ::= term (+ expr | \epsilon) \\
[/latex]

[latex]
term ::= factor (* term | \epsilon) \\
[/latex]

[latex]
factor ::= (expr) | nat \\
[/latex]

[latex]
nat ::= 0 | 1 | 2 | \dots \\
[/latex]

```tut:silent
object Arithmetics {
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
}

import Arithmetics._
```

```tut:silent
def eval(input: String): Either[String, Int] =
  expr.run(input) match {
    case Some(("", n))  => Right(n)
    case Some((out, _)) => Left(s"unconsumed input: $out")
    case None           => Left("invalid input")
  }
```

```tut:book
eval("2*(3+4)")
eval("2 * 3 +  4")
eval("2*(     3+ 4)  ")
eval("2*3")
eval(" (( 1 ))*( 2+( ( (   3) ) )* (4+(  ((5))+ 6))*( ((7*8   ))) +9)") 
```

```tut:book
val ps =
  List[Parser[Char]](
    char('a'),
    char('b'),
    lower,
    digit)

ps.foldK.run("foo")
``` 
