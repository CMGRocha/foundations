package exercises.dataprocessing

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double,         // sum of all temperatures in Fahrenheit
  size: Int            // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}

object Summary {

  val default: Summary =
    Summary(
      min = None,
      max = None,
      sum = 0.0,
      size = 0
    )

  def sampleOperation(state: Option[Sample], current: Option[Sample])(
    compare: (Double, Double) => Boolean
  ): Option[Sample] =
    (state, current) match {
      case (None, Some(_)) => current
      case (Some(_), None) => state
      case (Some(state), Some(value)) if compare(value.temperatureFahrenheit, state.temperatureFahrenheit) =>
        Some(value)
      case _ => state
    }

  val summaryMonoid: Monoid[Summary] = new Monoid[Summary] {
    override def default: Summary = Summary.default

    override def combine(first: Summary, second: Summary): Summary = Summary(
      //min = sampleOperation(first.min, second.min)((a, b) => a < b),
      //max = sampleOperation(first.max, second.max)((a, b) => a > b),
      min = Monoid.minSample.combine(first.min, second.min),
      max = Monoid.maxSample.combine(first.max, second.max),
      sum = first.sum + second.sum,
      size = first.size + second.size
    )
  }
}
