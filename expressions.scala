package templates

import cats.implicits._

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

sealed trait Value
case object `Unit` extends Value
case class Bool(underlying: Boolean) extends Value
case class Number(underlying: Int) extends Value
case class Text(underlying: String) extends Value

object ValuePrinter {
  def print(value: Value): String =
    value match {
      case `Unit` => ""
      case Bool(v) => v.toString
      case Number(v) => v.toString
      case Text(v) => v
    }
}

sealed trait Expression
case class Variable(name: String) extends Expression
case class Val(underlying: Value) extends Expression
case class If(condition: Expression, yes: Expression, no: Expression) extends Expression
case class Add(left: Expression, right: Expression) extends Expression
case class Mult(left: Expression, right: Expression) extends Expression
case class And(left: Expression, right: Expression) extends Expression
case class Not(expr: Expression) extends Expression

object ExpressionEvaluator {
  def eval(expr: Expression): Contexted[Value] =
    expr match {
      case Variable(name) =>
        for {
          found <- find(name)
          (expr, rest) = found
          value <- eval(expr)
          _ <- push(name, Val(value))
          _ <- merge(rest)
        } yield value

      case Val(value) =>
        result(value)

      case If(condition, yes, no) =>
        for {
          c <- eval(condition)
          cond <- c match {
            case Bool(b) => result(b)
            case _ => error(TypeError("Bool expected in If"))
          }
          result <- if (cond) eval(yes) else eval(no)
        } yield result

      case Add(left, right) =>
        for {
          l <- eval(left)
          r <- eval(right)
          add <- (l, r) match {
            case (Number(n), Number(m)) => result(Number(n + m))
            case _ => error(TypeError("Numbers expected in Add"))
          }
        } yield add

      case Mult(left, right) =>
        for {
          l <- eval(left)
          r <- eval(right)
          mult <- (l, r) match {
            case (Number(n), Number(m)) => result(Number(n * m))
            case _ => error(TypeError("Numbers expected in Mult"))
          }
        } yield mult

      case And(left, right) =>
        for {
          l <- eval(left)
          r <- eval(right)
          and <- (l, r) match {
            case (Bool(n), Bool(m)) => result(Bool(n && m))
            case _ => error(TypeError("Bools expected in And"))
          }
        } yield and

      case Not(expr) =>
        eval(expr).flatMap {
          case Bool(b) => result(Bool(!b))
          case _ => error(TypeError("Bool expected in Not"))
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
