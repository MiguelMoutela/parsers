package parsers

import org.scalatest._
import prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import cats.implicits._
import interpreters._
import Parsers._

class ParsersTests extends PropSpec with PropertyChecks with Matchers {

  private val nonEmptyString =
    Gen.nonEmptyListOf(arbitrary[Char]).map(_.mkString)

  property("anyChar should succeed with first char for non empty inputs") {
    forAll(nonEmptyString) { input =>
      anyChar.run(input) shouldEqual Right((input.tail, input.head))
    }
  }

  property("item should fail if input is empty") {
    anyChar.run("").toOption shouldEqual None
  }

  property("digit should succeed on numeric char") {
    forAll(Gen.numChar, Gen.alphaNumStr) { (c, out) =>
      digit.run(s"$c$out") shouldEqual Right((out, c.toInt))
    }
  }

  property("digit should fail on alpha char") {
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      digit.run(s"$c$out").toOption shouldEqual None
    }
  }

  property("char should succeed if input starts with specified char") {
    forAll(Arbitrary.arbitrary[Char], Gen.alphaNumStr) { (c, out) =>
      char(c).run(s"$c$out") shouldEqual Right((out, c))
    }
  }

  property("char should fail if input does not starts with specified char") {
    forAll(nonEmptyString) { input =>
      char((input.head.toInt + 1).toChar).run(input).toOption shouldEqual None
    }
  }

  property("string should succeed if input starts with specified string") {
    forAll(arbitrary[String], arbitrary[String]) { (str, out) =>
      val input = s"$str$out"
      string(str).run(input) shouldEqual Right((out, str))
    }
  }

  property("string should fail if input does not start with specified string") {
    forAll(nonEmptyString, arbitrary[String]) { (str, input) =>
      whenever(input.isEmpty || !input.startsWith(str)) {
        string(str).run(input).toOption shouldEqual None
      }
    }
  }

  property("many should always succeed") {
    forAll(Gen.choose(0, 20), arbitrary[Char], arbitrary[String]) {
      (n, c, out) =>
        whenever(out.isEmpty || !out.startsWith(s"$c")) {
          val start = (1 to n).map(_ => c).mkString
          val input = s"$start$out"
          many(char(c)).run(input) shouldEqual Right(
            (
              out,
              (1 to n).map(_ => c).toList
            ))
        }
    }
  }

  property("many1 should succeed with 'List(a, b, c)' for input 'abcFoo'") {
    many1(anyChar).run("abcFoo") shouldEqual Right(("", List('a', 'b', 'c', 'F', 'o', 'o')))
  }

  property("many1 should fail for empty inputs") {
    forAll(Gen.oneOf(anyChar, digit, char('f'))) { p =>
      many1(p).run("").toOption shouldEqual None
    }
  }

  property("nat") {
    nat.run("123foo") shouldEqual Right(("foo", 123))
  }

  property("nat fail") {
    forAll(Gen.alphaStr) { alphaStr =>
      nat.run(alphaStr).toOption shouldEqual None
    }
  }

  property("space") {
    space.run("   foo") shouldEqual Right(("foo", ()))
    space.run("foo") shouldEqual Right(("foo", ()))
  }

  property("token") {
    forAll(arbitrary[String]) { str =>
      token(string(str)).run(s"   $str   foo") shouldEqual Right(("foo", str))
      token(string(str)).run(s"   ${str}foo") shouldEqual Right(("foo", str))
      token(string(str)).run(s"$str foo") shouldEqual Right(("foo", str))
    }
  }

  property("token fail") {
    forAll(Gen.alphaUpperStr) { str =>
      token(digit).run(str).toOption shouldEqual None
    }
  }

  property("natural") {
    natural.run("  123 foo") shouldEqual Right(("foo", 123))
  }

  property("symbol") {
    symbol("foobar").run("  foobar foo") shouldEqual Right(("foo", "foobar"))
  }
}
