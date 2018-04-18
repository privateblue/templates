package object templates {
  sealed trait LanguageError {
    val msg: String
  }
  case class SyntaxError(val msg: String) extends LanguageError
  case class Undefined(val msg: String) extends LanguageError
  case class TypeError(val msg: String) extends LanguageError
  case class CyclicReference(val msg: String) extends LanguageError

  type Result[T] = Either[LanguageError, T]

  implicit class ErrorWith[T](result: Result[T]) {
    def errorWith[U](v: U): Either[(LanguageError, U), T] =
      result.fold(l => Left((l, v)), r => Right(r))
  }

  implicit class IgnoreWith[T, U](either: Either[(LanguageError, U), T]) {
    def ignoreWith: Result[T] = either match {
      case Left((err, _)) => Left(err)
      case Right(r) => Right(r)
    }
  }
}