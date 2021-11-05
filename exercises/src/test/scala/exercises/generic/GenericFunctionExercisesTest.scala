package exercises.generic

import exercises.generic.GenericFunctionExercises.Predicate.{alwaysFalse, alwaysTrue}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    forAll { (a: Int, b: Int) =>
      assert(Pair(a, b).swap == Pair(b, a))
    }
  }

  test("Pair map") {
    forAll { (a: Int, b: Int) =>
      assert(Pair(a, b).map(identity) == Pair(a, b))
    }
  }

  test("Pair decoded") {
  }

  test("Pair zipWith") {
    forAll { (a: Int, b: Int, x: Int, y: Int) =>
      assert(Pair(a, b).zipWith(Pair(x, y))(_ + _) == Pair(a + x, b + y))
    }
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  /*
   (isEven || isPositive)(12) == true
 (isEven || isPositive)(11) == true
 (isEven || isPositive)(-4) == true
(isEven || isPositive)(-7) == false
   */

  test("Predicate &&") {
    assert((isEven && isPositive) (12))
    assert(!(isEven && isPositive) (11))
    assert(!(isEven && isPositive) (-4))
    assert(!(isEven && isPositive) (-7))
  }

  test("Predicate && - property based testing") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      assert(!(p1 && alwaysFalse) (value))
      assert((p1 && alwaysTrue) (value) == p1(value))
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive) (12))
    assert((isEven || isPositive) (11))
    assert((isEven || isPositive) (-4))
    assert(!(isEven || isPositive) (-7))
  }

  test("Predicate || - property base testing") {
    forAll { (eval1: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)

      assert((p1 || alwaysTrue) (value))
      assert((p1 || alwaysFalse) (value) == p1(value))
    }
  }

  test("Predicate flip") {
    forAll { (value: Int) =>

      assert(alwaysFalse.flip(value))
      assert(!alwaysTrue.flip(value))
    }
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("x", 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1") == UserId(-1))

    assertThrows[IllegalArgumentException](userIdDecoder.decode("hello"))
  }

  test("JsonDecoder UserId - Property base testing - round trip") {
    forAll { (value: Int) =>
      val json = value.toString
      assert(userIdDecoder.decode(json) == UserId(value))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assertThrows[IllegalArgumentException](localDateDecoder.decode("2020-03-26"))
    assertThrows[IllegalArgumentException](localDateDecoder.decode("hello"))
  }

  val generatorLocalDate: Gen[LocalDate] =
    Gen.choose(min = LocalDate.MIN.toEpochDay, max = LocalDate.MAX.toEpochDay - 1)
      .map((x: Long) => LocalDate.ofEpochDay(x))

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] =
    Arbitrary(generatorLocalDate)

  test("JsonDecoder LocalDate - Property base testing - round trip") {
    forAll { (localDate: LocalDate) =>
      val dateStr = DateTimeFormatter.ISO_LOCAL_DATE.format(localDate)
      val json: String = s"\"$dateStr\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(weirdLocalDateDecoder.decode("18347") == LocalDate.of(2020, 3, 26))
    assertThrows[Exception](weirdLocalDateDecoder.decode("hello"))
  }

  test("JsonDecoder weirdLocalDateDecoder  - Property base testing - round trip") {
    forAll { (localDate: LocalDate) =>
      val dateStr = DateTimeFormatter.ISO_LOCAL_DATE.format(localDate)
      val json: String = s"\"$dateStr\""
      val json2: String = localDate.toEpochDay.toString
      assert(localDateDecoder.decode(json) == localDate)
      assert(localDateDecoder.decode(json2) == localDate)
    }
  }
}
