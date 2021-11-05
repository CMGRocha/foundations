package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]]) {
  def isEmpty: Boolean = partitions.forall(_.isEmpty)

  def toList: List[A] = partitions.flatten

  def map[To](update: A => To): ParList[To] =
    ParList(partitions.map(partition => partition.map(update)))

  def monoFoldLeft(param: Monoid[A]): A =
    partitions.map(partition => partition.foldLeft(param.default)(param.combine)).foldLeft(param.default)(param.combine)

  def size: Int = foldMap(_ => 1)(Monoid.sumInt)

  def mapReduce2[To](update: A => To)(monoid: Monoid[To]): To =
    map(update).monoFoldLeft(monoid)

  def foldMap[To](update: A => To)(monoid: Monoid[To]): To =
    partitions
      .map { partition =>
        partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
      }
      .foldLeft(monoid.default)(monoid.combine)

  def parFoldMap[To](update: A => To)(monoid: Monoid[To])(executionContext: ExecutionContext): To = {
    def foldPartition(partition: List[A]): Future[To] =
      Future {
        // println(s"[${Thread.currentThread().getName}] Start")
        val result = partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
         // println(s"[${Thread.currentThread().getName}] completed")
        result
      }(executionContext)

    partitions
      .map(foldPartition)
      .map(task => Await.result(task, Duration.Inf))
      .foldLeft(monoid.default)(monoid.combine)
  }

  def parFoldMapMine[To](update: A => To)(monoid: Monoid[To])(executionContext: ExecutionContext): To = {
    val future: Future[List[To]] = Future {
      partitions
        .map { partition =>
          partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
        }
    }(executionContext)
    val intermediateResult: Seq[To] = Await.result(future, Duration.Inf)
    intermediateResult.foldLeft(monoid.default)(monoid.combine)
  }

}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A]): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)
}
