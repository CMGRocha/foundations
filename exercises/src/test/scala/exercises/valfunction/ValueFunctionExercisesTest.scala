package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits my first property based test") {
    forAll { (text: String) =>
      assert(selectDigits(text).sum >= 0)
    }
  }

  test("selectDigits my second property based test") {
    forAll { (text: String) =>
      val digits = selectDigits(text)
      assertResult(digits)(digits.toUpperCase)
    }
  }
  test("selectDigits from solutions") {
    forAll { (text: String) =>
      selectDigits(text).foreach(x => assert(x.isDigit))
    }
  }

  test("secret unit test") {
    assert(secret("hello4world-80") == "**************")
  }

  test("secret length is the same") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret content is always *") {
    forAll { (text: String) =>
      secret(text).foreach(x => assert(x == '*'))
    }
  }

  test("secret content is idempotent") {
    forAll { (text: String) =>
      val once = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test("isValidUsername result should be the same for the input and its reverse") {
    forAll { (text: String) =>
      assert(isValidUsername(text) == isValidUsername(text.reverse))
    }
  }


  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point.isPositive should return when all the points are 0 or higher") {
    val zero = Point(0, 0, 0)
    val positive = Point(0, 0, 1)
    val negative = Point(0, 0, -1)
    assert(zero.isPositive)
    assert(positive.isPositive)
    assert(!negative.isPositive)
  }

  test("Point is positive receiving only positive numbers should return true") {
    forAll { (x: Int, y: Int, z: Int) =>
      // assert(Point(x.abs, y.abs, z.abs).isPositive) Int.MinValue.abs -> Still returns negative (JVM problem)
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test("Point is even if all points are divisible by two") {
    val even = Point(2, 2, 2)
    val odd = Point(2, 2, 3)
    assert(even.isEven)
    assert(!odd.isEven)
  }

  test("Point for all should calculate if all the pos are even") {
    val even = Point(2, 2, 2)
    assert(even.forAll(_ % 2 == 0))
  }

  test("Point for all using oracle methodology") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }
}
