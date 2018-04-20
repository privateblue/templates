import cats.data.StateT
import cats.implicits._

package object templates {
  sealed trait LanguageError {
    val msg: String
  }
  case class SyntaxError(val msg: String) extends LanguageError
  case class Undefined(val msg: String) extends LanguageError
  case class TypeError(val msg: String) extends LanguageError

  type Result[T] = Either[LanguageError, T]

  type Context = List[(String, Expression)]

  val EmptyContext: Context = List.empty

  type Contexted[T] = StateT[Result, Context, T]

  def lift[T](res: Result[T]): Contexted[T] =
    StateT.liftF(res)

  def result[T](v: T): Contexted[T] =
    StateT.pure(v)

  def error[T](err: LanguageError): Contexted[T] =
    StateT.liftF[Result, Context, T](Left(err))

  def set(ctx: Context): Contexted[Unit] =
    StateT.set[Result, Context](ctx)

  def get: Contexted[Context] =
    StateT.get[Result, Context]

  def push(name: String, expr: Expression): Contexted[Unit] =
    StateT.modify((name, expr) :: _)

  def merge(ctx: Context): Contexted[Unit] =
    StateT.modify(ctx ++ _)

  def find(
    name: String,
    after: Context = EmptyContext
  ): Contexted[(Expression, Context)] =
    get.flatMap {
      case Nil =>
        error(Undefined(s"Variable $name not found"))
      case (key, expr) :: before if key == name =>
        set(before).flatMap(_ => result((expr, after)))
      case head :: before =>
        set(before).flatMap(_ => find(name, after :+ head))
    }
}