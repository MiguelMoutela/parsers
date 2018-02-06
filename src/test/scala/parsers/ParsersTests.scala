package parsers

import org.scalatest._
import prop._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import cats.implicits._

class ParsersTests extends PropSpec with PropertyChecks with Matchers {

  private val nonEmptyString =
    Gen.nonEmptyListOf(arbitrary[Char]).map(_.mkString)
  private val nonAlphaChar = arbitrary[Char].suchThat(c => !c.isLetter)
  private val nonAlphaNumChar =
    arbitrary[Char].suchThat(c => !c.isLetterOrDigit)

  property("succeed should always succeed with the result value v") {
    forAll(nonEmptyString, arbitrary[Int]) { (input, v) =>
      v.pure[Parser].run(input) shouldEqual Some((input, v))
    }
  }

  property("item should succeed with first char for non empty inputs") {
    forAll(nonEmptyString) { input =>
      item.run(input) shouldEqual Some((input.tail, input.head))
    }
  }

  property("item should fail if input is empty") {
    item.run("") shouldEqual None
  }

  property("digit should succeed on numeric char") {
    forAll(Gen.numChar, Gen.alphaNumStr) { (c, out) =>
      digit.run(s"$c$out") shouldEqual Some((out, c.toInt))
    }
  }

  property("digit should fail on alpha char") {
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      digit.run(s"$c$out") shouldEqual None
    }
  }

  property("lower should succeed on lower case char") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      lower.run(s"$c$out") shouldEqual Some((out, c))
    }
  }

  property("lower should fail on upper case char") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      lower.run(s"$c$out") shouldEqual None
    }
  }

  property("lower should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      lower.run(s"$c$out") shouldEqual None
    }
  }

  property("upper should succeed on upper case char") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      upper.run(s"$c$out") shouldEqual Some((out, c))
    }
  }

  property("upper should fail on lower case char") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      upper.run(s"$c$out") shouldEqual None
    }
  }

  property("upper should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      upper.run(s"$c$out") shouldEqual None
    }
  }

  property("letter should succeed on alpha char") {
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      letter.run(s"$c$out") shouldEqual Some((out, c))
    }
  }

  property("letter should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      letter.run(s"$c$out") shouldEqual None
    }
  }

  property("alphaNum should succeed on alphaNum char") {
    forAll(Gen.alphaNumChar, Gen.alphaNumStr) { (c, out) =>
      alphaNum.run(s"$c$out") shouldEqual Some((out, c))
    }
  }

  property("alphaNum should fail on non alphaNum char") {
    forAll(nonAlphaNumChar, Gen.alphaNumStr) { (c, out) =>
      letter.run(s"$c$out") shouldEqual None
    }
  }

  property("char should succeed if input starts with specified char") {
    forAll(Arbitrary.arbitrary[Char], Gen.alphaNumStr) { (c, out) =>
      char(c).run(s"$c$out") shouldEqual Some((out, c))
    }
  }

  property("char should fail if input does not starts with specified char") {
    forAll(nonEmptyString) { input =>
      char((input.head.toInt + 1).toChar).run(input) shouldEqual None
    }
  }

  property("string should succeed if input starts with specified string") {
    forAll(arbitrary[String], arbitrary[String]) { (str, out) =>
      val input = s"$str$out"
      string(str).run(input) shouldEqual Some((out, str))
    }
  }

  property("string should fail if input does not start with specified string") {
    forAll(nonEmptyString, arbitrary[String]) { (str, input) =>
      whenever(input.isEmpty || !input.startsWith(str)) {
        string(str).run(input) shouldEqual None
      }
    }
  }

  property("many should always succeed") {
    forAll(Gen.choose(0, 20), arbitrary[Char], arbitrary[String]) {
      (n, c, out) =>
        whenever(out.isEmpty || !out.startsWith(s"$c")) {
          val start = (1 to n).map(_ => c).mkString
          val input = s"$start$out"
          many(char(c)).run(input) shouldEqual Some(
            (
              out,
              (1 to n).map(_ => c).toList
            ))
        }
    }
  }

  property("many1 should succeed with 'List(a, b, c)' for input 'abcFoo'") {
    many1(lower).run("abcFoo") shouldEqual Some(("Foo", List('a', 'b', 'c')))
  }

  property("many1 should fail for empty inputs") {
    forAll(Gen.oneOf(item, lower, upper, letter, char('f'))) { p =>
      many1(p).run("") shouldEqual None
    }
  }

  property("ident") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (x, xs) =>
      ident.run(s"$x$xs") shouldEqual Some(("", s"$x$xs"))
    }
  }

  property("ident fail") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (x, xs) =>
      ident.run(s"$x$xs") shouldEqual None
    }
  }

  property("nat") {
    nat.run("123foo") shouldEqual Some(("foo", 123))
  }

  property("nat fail") {
    forAll(Gen.alphaStr) { alphaStr =>
      nat.run(alphaStr) shouldEqual None
    }
  }

  property("space") {
    space.run("   foo") shouldEqual Some(("foo", ()))
    space.run("foo") shouldEqual Some(("foo", ()))
  }

  property("token") {
    forAll(arbitrary[String]) { str =>
      token(string(str)).run(s"   $str   foo") shouldEqual Some(("foo", str))
      token(string(str)).run(s"   ${str}foo") shouldEqual Some(("foo", str))
      token(string(str)).run(s"$str foo") shouldEqual Some(("foo", str))
    }
  }

  property("token fail") {
    forAll(Gen.alphaUpperStr) { str =>
      token(lower).run(str) shouldEqual None
    }
  }

  property("identifier") {
    identifier.run(" someIdentifier foo") shouldEqual Some(
      ("foo", "someIdentifier"))
  }

  property("natural") {
    natural.run("  123 foo") shouldEqual Some(("foo", 123))
  }

  property("symbol") {
    symbol("foobar").run("  foobar foo") shouldEqual Some(("foo", "foobar"))
  }

  property("monoid") {
    val ps = List(char('a'), char('b'), char('c'), lower, digit, item)
    ps.foldK.run("boo") shouldEqual Some(("oo", 'b'))
  }
}
