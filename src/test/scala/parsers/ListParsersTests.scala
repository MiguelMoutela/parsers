package parsers

import org.scalatest._
import prop._
import ListParsers._
import cats.data.NonEmptyList
import cats.implicits._

class ListParsersTests extends PropSpec with PropertyChecks with Matchers {
  val validLists =
    Table(
      ("input", "int list"),
      ("[1,5,3,5]", NonEmptyList(1, List(5, 3, 5))),
      ("  [   4 ,    5,6,7  ,  3 ,   9]", NonEmptyList(4, List(5, 6, 7, 3, 9))),
      ("[1]", NonEmptyList(1, Nil))
    )

  val invalidLists =
    Table(
      "input",
      "1,5,3,5]",
      "[1,5,3,5",
      "[1 5,3,5]",
      "[]",
      "[,]",
      "[4, 5,]"
    )

  property("nonEmptyIntList should parse valid lists correctly") {
    forAll(validLists) { (input: String, expected: NonEmptyList[Int]) =>
      val Some((rest, actual)) = nonEmptyIntList.run(input)
      actual shouldEqual expected
      rest shouldBe ""
    }
  }

  property("nonEmptyIntList should fail with invalid lists") {
    forAll(invalidLists) { input =>
      nonEmptyIntList.run(input) shouldEqual None
    }
  }
}
