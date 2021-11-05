package exercises.dataprocessing

import scala.annotation.tailrec
import scala.collection.mutable

object ForLoopExercises {

  def sum(numbers: List[Int]): Int = {
    foldLeft(numbers, 0)(_+_)
  }

  def sumMutable(numbers: List[Int]): Int = {
    var total = 0
    for (number <- numbers)
      total += number
    total
  }

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int = {
    foldLeft(items, 0)((a1: Int, _: A) => a1 + 1)
  }

  def sizeMutable[A](items: List[A]): Int = {
    var total = 0
    for (_ <- items)
      total += 1
    total
  }

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] = {
    foldLeft(numbers, None: Option[Int])((a1: Option[Int], a2: Int) => a1 match {
      case Some(value) => Some(value min a2)
      case None => Some(a2)
    })
  }

  def minMutable(numbers: List[Int]): Option[Int] = {
    if (size(numbers) == 0) None
    else {
      var currentMin = numbers.head
      for (number <- numbers)
        currentMin = currentMin min number
      Some(currentMin)
    }
  }

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] =
    foldLeft(words, Map.empty[String, Int])((x: Map[String, Int], y: String) => x.updatedWith(y) {
      case Some(value) => Some(value + 1)
      case None => Some(1)
    })


  def wordCountMutable(words: List[String]): Map[String, Int] = {
    val tempMap = mutable.Map[String, Int]()
    for (word <- words) {
      tempMap.updateWith(word) {
        case Some(value) => Some(value + 1)
        case None => Some(1)
      }
      // val currentWordCount = tempMap.getOrElse(word, 0) + 1
      // tempMap.update(word, currentWordCount)
    }
    tempMap.toMap
  }

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  // def pattern = ???
  @tailrec
  def foldLeft[From, To](as: List[From], initial: To)(f: (To, From) => To): To =
    as match {
      case ::(head, next) => foldLeft(next, f(initial, head))(f)
      case Nil => initial
    }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    foldLeft(elements, Nil: List[To])((acc: List[To], h: From) => acc :+ update(h))

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    foldLeft(elements, Nil: List[A])((acc: List[A], h: A) => h +: acc)

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    foldLeft(elements, None: Option[A])((_: Option[A], h: A) => Some(h))

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A])(ord: Ordering[A]): Option[A] =
    foldLeft(elements, Option.empty[A]) {
      case (None, element) => Some(element)
      case (Some(state), element) => Some(ord.min(state, element))
    }
}
