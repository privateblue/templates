package templates

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

object ExpressionParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical
  lexical.delimiters ++= Seq("(", ")", "+", "*")
  lexical.reserved += ("if", "then", "else", "true", "false", "and", "not")

  def parse(str: String): Result[Expression] = {
    val tokens = new lexical.Scanner(str)
    phrase(expression)(tokens) match {
      case Success(parsed, _) => Right(parsed)
      case NoSuccess(err, _) => Left(SyntaxError(err))
    }
  }

  lazy val expression: PackratParser[Expression] =
    conditional | not | add | mult | and | `val` | variable | parens

  lazy val conditional: PackratParser[If] =
    "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
      case "if" ~ condition ~ "then" ~ yes ~ "else" ~ no => If(condition, yes, no)
    }

  lazy val not: PackratParser[Not] =
    "not" ~ expression ^^ {
      case "not" ~ expr => Not(expr)
    }

  lazy val add: PackratParser[Add] =
    expression ~ "+" ~ expression ^^ {
      case left ~ "+" ~ right => Add(left, right)
    }

  lazy val mult: PackratParser[Mult] =
    expression ~ "*" ~ expression ^^ {
      case left ~ "*" ~ right => Mult(left, right)
    }

  lazy val and: PackratParser[And] =
    expression ~ "and" ~ expression ^^ {
      case left ~ "and" ~ right => And(left, right)
    }

  lazy val `val`: PackratParser[Val] =
    value ^^ {
      case v => Val(v)
    }

  lazy val variable: PackratParser[Variable] =
    ident ^^ {
      name => Variable(name)
    }

  lazy val parens: PackratParser[Expression] =
    "(" ~> expression <~ ")"

  lazy val value: PackratParser[Value] =
    bool | number | text

  lazy val text: PackratParser[Text] =
    stringLit ^^ { s => Text(s) }

  lazy val number: PackratParser[Number] =
    numericLit ^^ { l => Number(l.toInt) }

  lazy val bool: PackratParser[Bool] =
    boolT | boolF

  lazy val boolT: PackratParser[Bool] =
    "true" ^^ { _ => Bool(true) }

  lazy val boolF: PackratParser[Bool] =
    "false" ^^ { _ => Bool(false) }

}