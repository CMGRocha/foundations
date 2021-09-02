package answers.errorhandling

package object option {

  implicit class ListOptionExtension[A](options: List[Option[A]]) {
    def sequence: Option[List[A]] =
      OptionAnswers.sequence(options)
  }

  implicit class ListExtension[A](values: List[A]) {
    def traverse[B](transform: A => Option[B]): Option[List[B]] =
      OptionAnswers.traverse(values)(transform)
  }

}
