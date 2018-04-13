package object language {
  type Result[T] = Either[LanguageError, T]
}