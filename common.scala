package object language {
  sealed trait LanguageError {
    val msg: String
  }
  case class SyntaxError(val msg: String) extends LanguageError
  case class Undefined(val msg: String) extends LanguageError
  case class TypeError(val msg: String) extends LanguageError

  type Result[T] = Either[LanguageError, T]
}