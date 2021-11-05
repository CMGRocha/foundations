package exercises.generic

import exercises.generic.GenericFunctionExercises.JsonDecoder
import kantan.codecs.strings.StringDecoder

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object GenericFunctionExercises {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  val names: Pair[String] = Pair("John", "Elisabeth")
  val ages: Pair[Int] = Pair(32, 46)

  case class Pair[A](first: A, second: A) {
    // 1a. Implement `swap` which exchanges `first` and `second`
    // such as Pair("John", "Doe").swap == Pair("Doe", "John")
    def swap: Pair[A] =
      Pair(second, first)

    // 1b. Implement `map` which applies a function to `first` and `second`
    // such as Pair("John", "Doe").map(_.length) == Pair(4,3)
    def map[To](update: A => To): Pair[To] =
      Pair(update(first), update(second))

    // 1c. Implement `zipWith` which merges two Pairs using a `combine` function
    // such as Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6)
    //         Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World ")
    // Bonus: Why did we separate the arguments of `zipWith` into two set of parentheses?
    def zipWith[Other, To](other: Pair[Other])(combine: (A, Other) => To): Pair[To] =
      Pair(combine(first, other.first), combine(second, other.second))

    def map3[A2, A3, To](otherB: Pair[A2], otherC: Pair[A3])(combine: (A, A2, A3) => To): Pair[To] =
      zipWith(otherB)(((_: A), (_: A2)))
        .zipWith(otherC) { case ((a, b), c) => combine(a, b, c) }
  }

  // 1d. Use the Pair API to decode the content of `secret`.
  // Note: You can transform a Byte into a Char using `byte.toChar`
  //       or you can create a String from an Array[Byte] using `new String(byteArray)`
  // Note: You can remove the lazy keyword from `decoded` once you have implemented it.
  val secret: Pair[List[Byte]] =
  Pair(
    first = List(103, 110, 105, 109, 109, 97, 114, 103, 111, 114, 80),
    second = List(108, 97, 110, 111, 105, 116, 99, 110, 117, 70)
  )
  lazy val decoded: Pair[String] = // Pair(new String(secret.second.reverse.map(_.toChar).toArray), new String(secret.first.reverse.map(_.toChar).toArray))
    secret
      .map(x => new String(x.toArray))
      .map(_.reverse)
      .swap

  // 1e. Use the Pair API to combine `productNames` and `productPrices` into `products`
  // such as products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99))
  case class Product(name: String, price: Double)

  val productNames: Pair[String] = Pair("Coffee", "Plane ticket")
  val productPrices: Pair[Double] = Pair(2.5, 329.99)

  lazy val products: Pair[Product] =
    productNames.zipWith(productPrices)(Product)

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 1f. Can you implement a method on `Pair` similar to `zipWith`, but that combines 3
  // Pairs instead of 2? If yes, can you implement this method using `zipWith`?
  // Note: Libraries often call this method `map3` and `zipWith` is often called `map2`

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  val isPositive: Predicate[Int] =
    Predicate((number: Int) => number >= 0)

  val isEven: Predicate[Int] =
    Predicate((number: Int) => number % 2 == 0)

  lazy val isOddPositive: Predicate[Int] =
    isEven.flip && isPositive

  case class Predicate[A](eval: A => Boolean) {
    // DSL to call a predicate like a function
    // isPositive(10) instead of isPositive.eval(10)
    def apply(value: A): Boolean = eval(value)

    // 2a. Implement `&&` that combines two predicates using logical and
    // such as (isEven && isPositive)(12) == true
    // but     (isEven && isPositive)(11) == false
    //         (isEven && isPositive)(-4) == false
    //         (isEven && isPositive)(-7) == false
    def &&(other: Predicate[A]): Predicate[A] =
      Predicate(value => eval(value) && other.eval(value))

    // 2b. Implement `||` that combines two predicates using logical or
    // such as
    //
    //
    // but      e
    def ||(other: Predicate[A]): Predicate[A] =
      Predicate(value => eval(value) || other.eval(value))

    // 2c. Implement `flip` that reverses a predicate
    // such as isEven.flip(11) == true
    def flip: Predicate[A] =
      Predicate(!eval(_))

    def contravariantMap[To](zoom: To => A): Predicate[To] =
      Predicate(user => eval(zoom(user)))

    def contraMap[To](zoom: To => A): Predicate[To] = contravariantMap(zoom)
  }

  object Predicate {
    def alwaysFalse[A]: Predicate[A] = Predicate(_ => false)

    def alwaysTrue[A]: Predicate[A] = Predicate(_ => true)
  }

  // 2d. Implement `isValidUser`, a predicate which checks if a `User` is:
  // * an adult (older than 18 year) and
  // * their name is longer than or equal to 3 characters and
  // * their name is capitalized, meaning that it starts with an uppercase letter
  // such as isValidUser(User("John", 20)) == true
  // but     isValidUser(User("John", 17)) == false // user is not an adult
  //         isValidUser(User("john", 20)) == false // name is not capitalized
  //         isValidUser(User("x"   , 23)) == false // name is too small
  // Note: Can you use methods from the Predicate API such as `&&`, `||` or `flip`?
  // You may want to create new Predicate methods to improve the implementation of `isValidUser`.
  case class User(name: String, age: Int)

  lazy val isValidUser: Predicate[User] = {
    byUser(_.age)(isBiggerThan(18)) &&
      by[User, String](_.name)(isLongerThan(3) && isCapitalise)

    isAdult && validName && isCapitalize
  }

  def by[From, To](zoom: From => To)(subPredicate: Predicate[To]): Predicate[From] =
    subPredicate.contraMap(zoom)


  def byUser[To](zoom: User => To)(subPredicate: Predicate[To]): Predicate[User] =
  // Predicate(user => subPredicate(zoom(user)))
    subPredicate.contraMap(zoom)

  def isLongerThan(min: Int): Predicate[String] =
    isBiggerThan(min).contraMap(_.length)

  val isCapitalise: Predicate[String] =
    Predicate(_.charAt(0).isUpper)

  val isAdult: Predicate[User] = isBiggerThan(18).contraMap(_.age)
  val validName: Predicate[User] = isBiggerThan(3).contraMap(_.name.length)
  val isCapitalize: Predicate[User] = Predicate(_.name.charAt(0).isUpper)

  def isBiggerThan(min: Int): Predicate[Int] =
    Predicate(_ >= min)

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  // very basic representation of JSON
  type Json = String

  trait JsonDecoder[A] {
    outer =>
    def decode(json: Json): A

    /*
      def map[To](update: A => To): JsonDecoder[To] = {
        (j: Json) => update(decode(j))
      }*/
    def map[To](update: A => To): JsonDecoder[To] =
      (json: Json) => update(outer.decode(json))

    def orElse(other: JsonDecoder[A]): JsonDecoder[A] =
      (json: Json) => {
        val attempt = Try(outer.decode(json))
        attempt match {
          case Failure(_) => other.decode(json)
          case Success(value) => value
        }
      }
  }

  val longJsonDecoder: JsonDecoder[Long] = (json: Json) => json.toLong

  val intDecoder: JsonDecoder[Int] = new JsonDecoder[Int] {
    def decode(json: Json): Int = json.toInt
  }

  val stringDecoder: JsonDecoder[String] = new JsonDecoder[String] {
    def decode(json: Json): String =
      if (json.startsWith("\"") && json.endsWith("\"")) // check it starts and ends with `"`
        json.substring(1, json.length - 1)
      else
        throw new IllegalArgumentException(s"$json is not a valid JSON string")
  }

  // SAM syntax for JsonDecoder
  val intDecoderSAM: JsonDecoder[Int] =
    (json: Json) => json.toInt

  // 3a. Implement `userIdDecoder`, a `JsonDecoder` for the `UserId` case class
  // such as userIdDecoder.decode("1234") == UserId(1234)
  // but     userIdDecoder.decode("hello") would throw an Exception
  case class UserId(value: Int)

  lazy val userIdDecoder: JsonDecoder[UserId] =
  // mapCompanion(intDecoder)(UserId)
    intDecoder.map(UserId)

  lazy val userIdDecoder2: JsonDecoder[UserId] = new JsonDecoder[UserId] {
    override def decode(json: Json): UserId =
      try {
        UserId(intDecoder.decode(json))
      } catch {
        case _: Throwable => throw new IllegalArgumentException(s"$json is not a valid ID for User")
      }
  }

  // 3b. Implement `localDateDecoder`, a `JsonDecoder` for `LocalDate`
  // such as localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // but     localDateDecoder.decode("2020-03-26") would throw an Exception
  // and     localDateDecoder.decode("hello") would throw an Exception
  // Note: You can parse a `LocalDate` using `LocalDate.parse` with a java.time.format.DateTimeFormatter
  // e.g. DateTimeFormatter.ISO_LOCAL_DATE
  lazy val localDateDecoder: JsonDecoder[LocalDate] =
  // mapCompanion(stringDecoder)(content => LocalDate.parse(content, DateTimeFormatter.ISO_LOCAL_DATE))
  stringDecoder.map(LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE))

  lazy val localDateDecoder2: JsonDecoder[LocalDate] =
    (json: Json) => {
      val content = stringDecoder.decode(json)
      LocalDate.parse(content, DateTimeFormatter.ISO_LOCAL_DATE)
    }


  // 3c. Implement `map` a generic function that converts a `JsonDecoder` of `From`
  // into a `JsonDecoder` of `To`.
  // Bonus: Can you re-implement `userIdDecoder` and `localDateDecoder` using `map`
  def mapCompanion[From, To](decoder: JsonDecoder[From])(update: From => To): JsonDecoder[To] =
    (json: Json) => {
      update(decoder.decode(json))
    }

  // 3d. Move `map` inside of `JsonDecoder` trait so that we can use the syntax
  // `intDecoder.map(_ + 1)` instead of `map(intDecoder)(_ + 1)`

  // 3e. Imagine we have to integrate with a weird JSON API where dates are sometimes encoded
  // using a String with the format "yyyy-mm-dd" and sometimes they are encoded using
  // JSON numbers representing the number of days since the epoch. For example,
  // weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26)
  // weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26)
  // but weirdLocalDateDecoder.decode("hello") would throw an Exception
  // Try to think how we could extend JsonDecoder so that we can easily implement
  // other decoders that follow the same pattern.

  val longLocalDateDecoder: JsonDecoder[LocalDate] =
    longJsonDecoder.map(LocalDate.ofEpochDay)


  lazy val weirdLocalDateDecoder: JsonDecoder[LocalDate] =
    localDateDecoder orElse longLocalDateDecoder // Infix operator


  lazy val weirdLocalDateDecoder2: JsonDecoder[LocalDate] =
    (json: Json) => {
      val attempt = Try(localDateDecoder.decode(json))
      attempt match {
        case Failure(_) => longLocalDateDecoder.decode(json)
        case Success(value) => value
      }
    }
  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // 3f. How would you define and implement a `JsonDecoder` for a generic `Option`?
  // such as we can decode:
  // * "1" into a Some(1)
  // * "\"2020-08-03\"" into a Some(LocalDate.of(2020,08,3))
  // * "\"null\"" into a Some("null")
  // * "null" into "None"
  // Note: you may need to change the function signature
  def optionDecoder[A](decoder: JsonDecoder[A]): JsonDecoder[Option[A]] = {
    case "null" => None
    case value: String => Some(decoder.decode(value))
  }

  // 3g. `JsonDecoder` currently throws an exception if the input is not a valid JSON.
  // How could you change the API so that it doesn't happen anymore?

  trait SafeJsonDecoder[A] {
    self =>
    def decode(json: Json): Either[Throwable, A]

    def map[To](update: A => To): SafeJsonDecoder[To] = {
      (json: Json) => self.decode(json).map(update)
    }

    def orElse(other: SafeJsonDecoder[A]): SafeJsonDecoder[A] =
      (json: Json) => self.decode(json).orElse(other.decode(json))
  }

  object SafeJsonDecoder {
    val int: SafeJsonDecoder[Int] =
      (json: Json) => Try(json.toInt) match {
        case Failure(exception) => Left(exception)
        case Success(value) => Right(value)
      }

    val long: SafeJsonDecoder[Long] =
      (json: Json) => Try(json.toLong) match {
        case Failure(exception) => Left(exception)
        case Success(value) => Right(value)
      }

    val string: SafeJsonDecoder[String] =
      (json: Json) =>
        if (json.startsWith("\"") && json.endsWith("\"")) // check it starts and ends with `"`
          Right(json.substring(1, json.length - 1))
        else
          Left(throw new IllegalArgumentException(s"$json is not a valid JSON string"))

    val localDateInt: SafeJsonDecoder[LocalDate] =
      long.map(LocalDate.ofEpochDay)

    val localDateString: SafeJsonDecoder[LocalDate] =
      string.map(LocalDate.parse(_, DateTimeFormatter.ISO_LOCAL_DATE))

    val localDate: SafeJsonDecoder[LocalDate] =
      localDateString orElse localDateInt

  }


}
