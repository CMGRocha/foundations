package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }
  test("size 2") {
    val size1 = size(List())
    val size2 = size(List())
    val newList = List() ++ List()

    assert(size(newList) == (size1 + size2))
  }
  test("size - property base test ") {
    forAll { (numbers: List[Int]) =>
      assert(size(numbers) == numbers.length)
    }
  }

  test("size - property base test 2") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      val size1 = size(l1)
      val size2 = size(l2)
      assert(size(l1 ++ l2) == (size1 + size2))
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min - property base test ") {
    forAll { (numbers: List[Int]) =>
      whenever(numbers.nonEmpty) {
        assert(min(numbers).get == numbers.min)
      }
    }
  }

  test("min - property base test - video") {
    forAll { (numbers: List[Int]) =>
      for {
        minValue <- min(numbers)
        number <- numbers
      } assert(minValue <= number)
    }
  }

  test("min - returned value belongs to the list") {
    forAll { (numbers: List[Int]) =>
      min(numbers).foreach(minValue => assert(numbers.contains(minValue)))
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount always returns positive values") {
    forAll { (words: List[String]) =>
      wordCount(words).values.foreach(value => assert(value > 0))
    }
  }

  test("wordCount keys always returns values in the original list") {
    forAll { (words: List[String]) =>
      wordCount(words).keys.foreach(key => assert(words.contains(key)))
    }
  }

  test("fold left process inputs in order") {
    forAll { (words: List[String]) =>
      val result = foldLeft(words, List.empty[String])((prefix, word) => prefix :+ word)
      assert(result == words)
    }
  }

  test("map") {
    val numbers = List(1, 2, 3)
    val result: List[Int] = map(numbers)(_ + 1)
    assert(result == List(2, 3, 4))
  }

  test("reverse") {
    val numbers = List(1, 2, 3)
    val result: List[Int] = reverse(numbers)
    assert(result == List(3, 2, 1))
  }

  test("last option") {
    assert(lastOption(List(3, 8, 1)) == Some(1))
    assert(lastOption(Nil) == None)
  }
}
