package exercises.errorhandling.validation

import exercises.errorhandling.NEL
import exercises.errorhandling.validation.Validation.Invalid
import exercises.errorhandling.validation.ValidationExercises.Country._
import exercises.errorhandling.validation.ValidationExercises.FormError._
import exercises.errorhandling.validation.ValidationExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValidationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("validateCountry example") {
    assert(validateCountry("FRA") == France.valid)
    assert(validateCountry("UK") == InvalidFormat("UK").invalid)
    assert(validateCountry("ARG") == NotSupported("ARG").invalid)
  }

  test("checkUsernameSize example") {
    assert(checkUsernameSize("bob_2167") == ().valid)
    assert(checkUsernameSize("bob_2") == ().valid)
    assert(checkUsernameSize("bo") == TooSmall(2).invalid)
  }

  test("checkUsernameCharacters example") {
    assert(checkUsernameCharacters("_abc-123_") == ().valid)
    assert(checkUsernameCharacters("foo!~23}AD") == InvalidCharacters(NEL('!', '~', '}')).invalid)
  }

  test("validateUsername example") {
    assert(validateUsername("bob_2167") == Username("bob_2167").valid)
    assert(validateUsername("bo") == TooSmall(2).invalid)
    assert(validateUsername("foo!~23}AD") == InvalidCharacters(NEL('!', '~', '}')).invalid)
    assert(validateUsername("!") == NEL(TooSmall(1), InvalidCharacters(NEL('!'))).invalid)
  }

  test("validateUser example") {
    assert(validateUser("bob_2167", "FRA") == User(Username("bob_2167"), France).valid)
    assert(validateUser("bob_2167", "UK") == InvalidFormat("UK").invalid)
    assert(validateUser("bo", "FRA") == TooSmall(2).invalid)
    assert(validateUser("b!", "UK") == NEL(TooSmall(2), InvalidCharacters(NEL('!')), InvalidFormat("UK")).invalid)
  }

  test("validateUserImproved with field error description") {
    val result = Invalid(
      NEL(
        FieldError(FieldIds.username, NEL(TooSmall(2), InvalidCharacters(NEL('!')))),
        FieldError(FieldIds.countryOfResidence, NEL(InvalidFormat("UK")))
      )
    )
    assert(validateUserImproved("b!", "UK") == result)
  }

}
