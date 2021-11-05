package exercises.dataprocessing

import exercises.dataprocessing
import exercises.dataprocessing.ThreadPoolUtil.fixedSizeExecutionContext

object TemperatureExercises {
  val ec4Cores = fixedSizeExecutionContext(4)

  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`

  object MinSampleComparison {
    implicit val sampleTemperatureCelsiusOrdering: Ordering[(Sample, Double)] =
      Ordering.fromLessThan((a, b) => a._2.compareTo(b._2) < 0)

    implicit val temperatureCelsiusOrdering: Ordering[Sample] =
      Ordering.fromLessThan((a, b) => a.temperatureCelsius.compareTo(b.temperatureCelsius) < 0)
  }

  import exercises.dataprocessing.TemperatureExercises.MinSampleComparison.{
    sampleTemperatureCelsiusOrdering,
    temperatureCelsiusOrdering
  }

  def minSampleByTemperatureMine(samples: ParList[Sample]): Option[Sample] =
    samples.partitions
      .map(partitionWithCelsius => partitionWithCelsius.zip(partitionWithCelsius.map(_.temperatureCelsius)))
      .flatMap(local => local.minOption)
      .map(_._1)
      .minOption

  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] =
    // samples.foldMap(sample => Option(sample): Option[Sample])(Monoid.minSample: Monoid[Option[Sample]])
    //samples.foldMap(Option(_))(Monoid.minSample)
    samples.parFoldMap(Option(_))(Monoid.minSample)(ec4Cores)

  def minSampleByTemperatureOld(samples: ParList[Sample]): Option[Sample] =
    minSampleByTemperatureCelsiusFromSample(
      samples.partitions.flatMap(minSampleByTemperatureCelsiusFromSample)
    )

  def minSampleByTemperatureCelsiusFromSample(samples: List[Sample]): Option[Sample] =
    samples.foldLeft(Option.empty[Sample]) {
      case (None, sample) => Some(sample)
      case (Some(previousMinSample), sample) if previousMinSample.temperatureCelsius > sample.temperatureCelsius =>
        Some(sample)
      case (optPreviousMin, _) => optPreviousMin
    }

  def minOption(state: Option[Sample], sample: Sample): Option[Sample] =
    (state, sample) match {
      case (None, sample) => Some(sample)
      case (Some(previousMinSample), sample) if previousMinSample.temperatureCelsius > sample.temperatureCelsius =>
        Some(sample)
      case (optPreviousMin, _) => optPreviousMin
    }

  def minOption(state: Option[Sample], current: Option[Sample]): Option[Sample] =
    (state, current) match {
      case (None, Some(_))                                                                   => current
      case (Some(_), None)                                                                   => state
      case (Some(state), Some(value)) if value.temperatureCelsius < state.temperatureCelsius => Some(value)
      case _                                                                                 => state
    }

  def minSampleByTemperatureFoldLeft(samples: ParList[Sample]): Option[Sample] =
    foldLeft(samples, Option.empty[Sample])((state: Option[Sample], sample: Sample) => minOption(state, sample))(
      (state: Option[Sample], other: Option[Sample]) => minOption(state, other)
    )

  def sumTemperaturesFoldLeft(samples: ParList[Sample]): Double =
    foldLeft(samples, 0.0)(combineElements = (state, sample) => state + sample.temperatureFahrenheit)(
      combineIntermediateResults = _ + _
    )

  def sumTemperatures(samples: ParList[Sample]): Double =
    samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble)(ec4Cores)

  def sizeOriginal(samples: ParList[Sample]): Int =
    samples.partitions.map(_.size).sum

  def sizeOutside(samples: ParList[Sample]): Int =
    samples.parFoldMap(_ => 1)(Monoid.sumInt)(ec4Cores)

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def averageTemperatureOld(samples: ParList[Sample]): Option[Double] =
    aggregateTuple(samples.partitions.flatMap(calculateNumberOccurrencesAndSumOfOccurrences))
      .map(tuple => tuple._2 / tuple._1)

  def calculateNumberOccurrencesAndSumOfOccurrences(samples: List[Sample]): Option[(Int, Double)] =
    samples.foldLeft(Option.empty[(Int, Double)]) {
      case (None, sample) => Some(1, sample.temperatureFahrenheit)
      case (Some(previousTuple), sample) =>
        Some((previousTuple._1 + 1, previousTuple._2 + sample.temperatureFahrenheit))
    }

  def aggregateTuple(samples: List[(Int, Double)]): Option[(Int, Double)] =
    samples.foldLeft(Option.empty[(Int, Double)]) {
      case (None, tuple)                  => Some(tuple)
      case (Some(previousTuple), current) => Some((previousTuple._1 + current._1, previousTuple._2 + current._2))
    }

  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
    val (temperatures, size): (Double, Int) = samples
      .parFoldMap(sample => (sample.temperatureFahrenheit, 1))(Monoid.zip(Monoid.sumDouble, Monoid.sumInt))(ec4Cores)

    if (size == 0) None
    else Some(temperatures / size)
  }

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  def foldLeft[From, To](parList: ParList[From], default: To)(
    combineElements: (To, From) => To
  )(combineIntermediateResults: (To, To) => To): To =
    parList.partitions
      .map(partition => partition.foldLeft(default)(combineElements))
      .foldLeft(default)(combineIntermediateResults)

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  def monoFoldLeftOutside[A](parList: ParList[A], default: A)(combine: (A, A) => A): A =
    parList.partitions.map(partition => partition.foldLeft(default)(combine)).foldLeft(default)(combine)

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parFoldMap(Option(_))(Monoid.minSample)(ec4Cores),
      max = samples.parFoldMap(Option(_))(Monoid.maxSample)(ec4Cores),
      sum = samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble)(ec4Cores),
      size = samples.parFoldMap(_ => 1)(Monoid.sumInt)(ec4Cores)
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    samples.parFoldMap(sampleToSummary)(Summary.summaryMonoid)(ec4Cores)

  def sampleToSummary(sample: Sample): Summary =
    Summary(
      min = Option(sample),
      max = Option(sample),
      sum = sample.temperatureFahrenheit,
      size = 1
    )

  // type Output = Map[String, Summary]
  def sampleToOutput(sample: Sample): Map[String, Summary] =
    Map(
      sample.city    -> sampleToSummary(sample),
      sample.country -> sampleToSummary(sample)
    )

  val monoidOutput: Monoid[Map[String, Summary]] = new Monoid[Map[String, Summary]] {
    override def default: Map[String, Summary] = Map.empty

    override def combine(first: Map[String, Summary], second: Map[String, Summary]): Map[String, Summary] =
      second.foldLeft(first) { case (state: Map[String, Summary], (city, summary)) =>
        state.updatedWith(city) {
          case None                 => Some(summary)
          case Some(currentSummary) => Some(Summary.summaryMonoid.combine(currentSummary, summary))
        }
      }
    /* ALTERNATIVE
      second.foldLeft(first) { case (state, (city, summary)) =>
        state.get(city) match {
          case None                 => state.updated(city, summary)
          case Some(currentSummary) => state.updated(city, Summary.summaryMonoid.combine(currentSummary, summary))
        }
      }
     */

  }

  def aggregateByCity(samples: ParList[Sample]): Map[String, Summary] =
    samples.parFoldMap(sampleToOutput)(monoidOutput)(ec4Cores)

  def sampleToOutput(keys: Sample => List[String])(sample: Sample): Map[String, Summary] =
    keys(sample)
      .map(key => key -> sampleToSummary(sample))
      .toMap

  def aggregateByLabel(samples: ParList[Sample])(keys: Sample => List[String]): Map[String, Summary] =
    samples.parFoldMap(sample => sampleToOutput(keys)(sample))(monoidOutput)(ec4Cores)

}
