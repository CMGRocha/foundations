package exercises.dataprocessing

/** Please use a default value such as when used with combine it is a no-operation
  *
  * @tparam A a
  */
trait Monoid[A] {
  def default: A

  def combine(first: A, second: A): A
}

object Monoid {
  val sumInt: Monoid[Int] = new Monoid[Int] {
    override def default: Int = 0

    override def combine(first: Int, second: Int): Int = first + second
  }

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    override def default: Double = 0.0d

    override def combine(first: Double, second: Double): Double = first + second
  }

  val sumDoubleIntTuple: Monoid[(Double, Int)] = new Monoid[(Double, Int)] {
    override def default: (Double, Int) = (sumDouble.default, sumInt.default)

    override def combine(first: (Double, Int), second: (Double, Int)): (Double, Int) =
      (sumDouble.combine(first._1, second._1), sumInt.combine(first._2, second._2))
  }

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def default: (A, B) = (monoidA.default, monoidB.default)

      override def combine(first: (A, B), second: (A, B)): (A, B) =
        (monoidA.combine(first._1, second._1), monoidB.combine(first._2, second._2))
    }

  def compareSample(compare: (Sample, Sample) => Sample): Monoid[Option[Sample]] = new Monoid[Option[Sample]] {
    override def default: Option[Sample] = None

    override def combine(first: Option[Sample], second: Option[Sample]): Option[Sample] =
      (first, second) match {
        case (None, Some(_))            => second
        case (Some(_), None)            => first
        case (Some(state), Some(value)) => Some(compare(state, value))
        case _                          => first
      }
  }

  val minSample: Monoid[Option[Sample]] = {
    val compare: (Sample, Sample) => Sample = (sample1, sample2) =>
      if (sample2.temperatureFahrenheit < sample1.temperatureFahrenheit) sample2 else sample1
    compareSample(compare)
  }

  val maxSample: Monoid[Option[Sample]] = {
    val compare: (Sample, Sample) => Sample = (sample1, sample2) =>
      if (sample2.temperatureFahrenheit > sample1.temperatureFahrenheit) sample2 else sample1
    compareSample(compare)
  }



}
