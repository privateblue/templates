package templates

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

sealed trait Value
case class Bool(underlying: Boolean) extends Value
case class Number(underlying: Int) extends Value
case class Text(underlying: String) extends Value

sealed trait Expression
case class Variable(name: String) extends Expression
case class Val(underlying: Value) extends Expression
case class If(condition: Expression, yes: Expression, no: Expression) extends Expression
case class Add(left: Expression, right: Expression) extends Expression
case class Mult(left: Expression, right: Expression) extends Expression
case class And(left: Expression, right: Expression) extends Expression
case class Not(expr: Expression) extends Expression

object ExpressionEvaluator {
  type Context = List[(String, Expression)]

  val EmptyContext: Context = List.empty

  def put(name: String, expr: Expression, ctx: Context): Context =
    (name, expr) :: ctx

  def find(
    name: String,
    ctx: Context,
    prefix: Context = EmptyContext
  ): Result[(Context, Context)] =
    ctx match {
      case Nil => Left(Undefined(s"Variable $name not found"))
      case (key, expr) :: rest if key == name => Right((prefix, ctx))
      case head :: rest => find(name, rest, prefix :+ head)
    }

  def eval(expr: Expression, ctx: Context): Result[(Value, Context)] =
    expr match {
      case Variable(name) =>
        for {
          split <- find(name, ctx)
          (rest, (_, expr) :: ctx1) = split
          evaluated <- eval(expr, ctx1)
          (value, ctx2) = evaluated
          binding = (name, Val(value))
          ctx3 = binding :: ctx2
        } yield (value, rest ++ ctx3)

      case Val(value) =>
        Right((value, ctx))

      case If(condition, yes, no) =>
        for {
          condEval <- eval(condition, ctx)
          (cond, ctx1) = condEval
          c <- cond match {
            case Bool(b) => Right(b)
            case _ => Left(TypeError("Bool expected in If"))
          }
          result <- if (c) eval(yes, ctx1) else eval(no, ctx1)
        } yield result

      case Add(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Number(n), Number(m)) => Right((Number(n + m), ctx2))
            case _ => Left(TypeError("Numbers expected in Add"))
          }
        } yield result

      case Mult(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Number(n), Number(m)) => Right((Number(n * m), ctx2))
            case _ => Left(TypeError("Numbers expected in Mult"))
          }
        } yield result

      case And(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Bool(n), Bool(m)) => Right((Bool(n && m), ctx2))
            case _ => Left(TypeError("Bools expected in And"))
          }
        } yield result

      case Not(expr) =>
        eval(expr, ctx).flatMap {
          case (Bool(b), ctx1) => Right((Bool(!b), ctx1))
          case _ => Left(TypeError("Bool expected in Not"))
        }
    }
}

trait ExpressionParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical
  lexical.delimiters ++= Seq("(", ")", "+", "*")
  lexical.reserved += ("if", "then", "else", "true", "false", "and", "not")

  lazy val expression: PackratParser[Expression] =
    conditional | not | add | mult | and | `val` | variable | parens

  lazy val conditional: PackratParser[If] =
    "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
      case _ ~ condition ~ _ ~ yes ~ _ ~ no => If(condition, yes, no)
    }

  lazy val not: PackratParser[Not] =
    "not" ~ expression ^^ {
      case _ ~ expr => Not(expr)
    }

  lazy val add: PackratParser[Add] =
    expression ~ "+" ~ expression ^^ {
      case left ~ _ ~ right => Add(left, right)
    }

  lazy val mult: PackratParser[Mult] =
    expression ~ "*" ~ expression ^^ {
      case left ~ _ ~ right => Mult(left, right)
    }

  lazy val and: PackratParser[And] =
    expression ~ "and" ~ expression ^^ {
      case left ~ _ ~ right => And(left, right)
    }

  lazy val `val`: PackratParser[Val] =
    value ^^ Val.apply

  lazy val variable: PackratParser[Variable] =
    ident ^^ Variable.apply

  lazy val parens: PackratParser[Expression] =
    "(" ~> expression <~ ")"

  lazy val value: PackratParser[Value] =
    bool | number | text

  lazy val text: PackratParser[Text] =
    stringLit ^^ Text.apply

  lazy val number: PackratParser[Number] =
    numericLit ^^ { l => Number(l.toInt) }

  lazy val bool: PackratParser[Bool] =
    boolT | boolF

  lazy val boolT: PackratParser[Bool] =
    "true" ^^ { _ => Bool(true) }

  lazy val boolF: PackratParser[Bool] =
    "false" ^^ { _ => Bool(false) }
}
