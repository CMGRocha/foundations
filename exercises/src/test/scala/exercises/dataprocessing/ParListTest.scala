package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.ExecutionContext

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature returns the same sample by parList or in a single list") {
    forAll { (samples: ParList[Sample]) =>
      assert(minSampleByTemperature(samples) == samples.toList.minByOption(_.temperatureCelsius))
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperatureOld(parSamples) == Some(53.6))
  }

  test("averageTemperature property base testing") {
    forAll { (samples: ParList[Sample]) =>
      val originalAvg = averageTemperatureOld(samples)
      val doubleAvg = averageTemperatureOld(
        samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit * 2))
      )
      if (originalAvg.isEmpty) assert(samples.isEmpty)
      else assert((doubleAvg.getOrElse(0d) - originalAvg.getOrElse(1d) * 2).abs < 0.00001)
    }
  }

  test("monoFoldLeft sum") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(Monoid.sumInt) == numbers.toList.sum)
    }
  }

  test(" MonoFoldParam sumInt - combine to be a no-op with default") {
    forAll { (number: Int) =>
      assert(Monoid.sumInt.combine(number, Monoid.sumInt.default) == number)
      assert(Monoid.sumInt.combine(Monoid.sumInt.default, number) == number)
    }
  }

  test("foldMap is consistent with map followed by MonoFoldLeft") {
    forAll { (numbers: ParList[Int]) =>
      val monoid             = Monoid.sumInt
      val foldMapResult: Int = numbers.foldMap(identity)(monoid)
      // val mapPusMonoFoldLeft = numbers.map(identity).monoFoldLeft(monoid)
      val mapPusMonoFoldLeft = numbers.monoFoldLeft(monoid)

      assert(foldMapResult == mapPusMonoFoldLeft)
    }
  }

  test("Slowish foldMap is consistent with map followed by MonoFoldLeft") {
    forAll { (numbers: ParList[String], update: String => Int) =>
      val monoid             = Monoid.sumInt
      val foldMapResult: Int = numbers.foldMap(update)(monoid)
      val mapPusMonoFoldLeft = numbers.map(update).monoFoldLeft(monoid)
      assert(foldMapResult == mapPusMonoFoldLeft)
    }
  }

  def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  test("parFoldMap should have the same result as foldMap") {
    forAll { (numbers: ParList[Int]) =>
      val monoid             = Monoid.sumInt
      val foldMapResult: Int = numbers.foldMap(identity)(monoid)
      val paraFoldMap: Int   = numbers.parFoldMap(identity)(monoid)(executionContext)

      assert(paraFoldMap == foldMapResult)
    }
  }

  /*
  implicit val doubleArab: Arbitrary[Double] =
    Arbitrary(Gen.choose(-100.0f, 100.0f).map(_.toDouble))
   */
  val doubleGen: Gen[Double]                = Gen.choose(-100.0f, 100.0f).map(_.toDouble)
  val intGen: Gen[Int]                      = Gen.choose(Int.MinValue, Int.MaxValue)
  val tupleDoubleIntGen: Gen[(Double, Int)] = Gen.zip(doubleGen, intGen)

  checkMonoid("sumInt", Monoid.sumInt, intGen)
  checkMonoid("sumDouble", Monoid.sumDouble, doubleGen)
  checkMonoid("sumDoubleIntTuple", Monoid.sumDoubleIntTuple, tupleDoubleIntGen)
  checkMonoid("zipDoubleInt", Monoid.zip(Monoid.sumDouble, Monoid.sumInt), tupleDoubleIntGen)
  checkMonoid("minSample", Monoid.minSample, Gen.option(sampleGen))
  checkMonoid("maxSample", Monoid.maxSample, Gen.option(sampleGen))


  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  def checkMonoid[A](name: String, param: Monoid[A], gen: Gen[A]): Unit = {
    test(s" MonoFoldParam $name - combine to be a no-op with default generalized") {
      forAll(gen) { (value: A) =>
        assert(param.combine(value, param.default) == value)
        assert(param.combine(param.default, value) == value)
      }
    }
    test(s" MonoFoldParam $name - is associative") {
      forAll(gen, gen, gen) { (first: A, second: A, third: A) =>
        val oneWay   = param.combine(first, param.combine(second, third))
        val otherWay = param.combine(param.combine(first, second), third)
        assert(oneWay == otherWay)
      }
    }
  }
  /*
    def checkMonoid[A: Arbitrary](name: String, param: Monoid[A]): Unit = { // (implicit arb : Arbitrary[A])
      test(s" MonoFoldParam $name - combine to be a no-op with default generalized") {
        forAll { (value: A) =>
          assert(param.combine(value, param.default) == value)
          assert(param.combine(param.default, value) == value)
        }
      }
      test(s" MonoFoldParam $name - is associative") {
        forAll { (first: A, second: A, third: A) =>
          val oneWay = param.combine(first, param.combine(second, third))
          val otherWay = param.combine(param.combine(first, second), third)
          assert(oneWay == otherWay)
        }
      }
    }
   */

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
