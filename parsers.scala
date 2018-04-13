import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

sealed trait PExpression
case class PVariable(name: String) extends PExpression
case class PBoolValue(underlying: Boolean) extends PExpression
case class PNumberValue(underlying: Int) extends PExpression
case class PTextValue(underlying: String) extends PExpression
case class PIf(condition: PExpression, yes: PExpression, no: PExpression) extends PExpression
case class PAdd(left: PExpression, right: PExpression) extends PExpression
case class PMult(left: PExpression, right: PExpression) extends PExpression
case class PAnd(left: PExpression, right: PExpression) extends PExpression
case class PNot(expr: PExpression) extends PExpression

object PExpression {
  def toExpression(pexpr: PExpression) = {
    val i = toInt(pexpr)
    val b = toBoolean(pexpr)
    val s = toString(pexpr)
    val expr = List(i.toList,b.toList,s.toList).flatten.headOption
    expr.getOrElse(throw new Exception("Type error"))
  }

  def toInt(pexpr: PExpression): Option[Expression[Int]] =
    pexpr match {
      case PVariable(name) => Some(Variable(name))
      case PNumberValue(underlying) => Some(Value[Int](underlying))
      case PAdd(left, right) => for {
        l <-toInt(left)
        r <- toInt(right)
      } yield Add(l, r)
      case PMult(left, right) => for {
        l <-toInt(left)
        r <- toInt(right)
      } yield Mult(l, r)
      case PIf(condition, yes, no) => for {
        c <- toBoolean(condition)
        y <-toInt(yes)
        n <- toInt(no)
      } yield If(c, y, n)
      case _ => None
    }

  def toBoolean(pexpr: PExpression): Option[Expression[Boolean]] =
    pexpr match {
      case PVariable(name) => Some(Variable(name))
      case PBoolValue(underlying) => Some(Value[Boolean](underlying))
      case PAnd(left, right) => for {
        l <-toBoolean(left)
        r <- toBoolean(right)
      } yield And(l, r)
      case PNot(expr) => toBoolean(expr).map(Not.apply)
      case PIf(condition, yes, no) => for {
        c <- toBoolean(condition)
        y <-toBoolean(yes)
        n <- toBoolean(no)
      } yield If(c, y, n)
      case _ => None
    }

  def toString(pexpr: PExpression): Option[Expression[String]] =
    pexpr match {
      case PVariable(name) => Some(Variable(name))
      case PTextValue(underlying) => Some(Value[String](underlying))
      case PIf(condition, yes, no) => for {
        c <- toBoolean(condition)
        y <-toString(yes)
        n <- toString(no)
      } yield If(c, y, n)
      case _ => None
    }
}

object ExpressionParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical
  lexical.delimiters ++= Seq("(", ")", "+", "*")
  lexical.reserved += ("if", "then", "else", "true", "false", "and", "not")

  def parse(str: String) = {
    val tokens = new lexical.Scanner(str)
    phrase(expression)(tokens) match {
      case Success(parsed, _) => PExpression.toExpression(parsed)
      case NoSuccess(err, _) => throw new Exception(err)
    }
  }

  lazy val expression: PackratParser[PExpression] =
    conditional | not | add | mult | and | value | variable | parens

  lazy val value = bool | number

  lazy val text: PackratParser[PTextValue] =
    stringLit ^^ { s => PTextValue(s) }

  lazy val number: PackratParser[PNumberValue] =
    numericLit ^^ { l => PNumberValue(l.toInt) }

  lazy val bool: PackratParser[PBoolValue] =
    boolT | boolF

  lazy val boolT: PackratParser[PBoolValue] =
    "true" ^^ { _ => PBoolValue(true) }

  lazy val boolF: PackratParser[PBoolValue] =
    "false" ^^ { _ => PBoolValue(false) }

  lazy val conditional: PackratParser[PIf] =
    "if" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
      case "if" ~ condition ~ "then" ~ yes ~ "else" ~ no => PIf(condition, yes, no)
    }

  lazy val add: PackratParser[PAdd] =
    expression ~ "+" ~ expression ^^ {
      case left ~ "+" ~ right => PAdd(left, right)
    }

  lazy val mult: PackratParser[PMult] =
    expression ~ "*" ~ expression ^^ {
      case left ~ "*" ~ right => PMult(left, right)
    }

  lazy val and: PackratParser[PAnd] =
    expression ~ "and" ~ expression ^^ {
      case left ~ "and" ~ right => PAnd(left, right)
    }

  lazy val not: PackratParser[PNot] =
    "not" ~ expression ^^ {
      case "not" ~ expr => PNot(expr)
    }

  lazy val variable: PackratParser[PVariable] =
    ident ^^ {
      name => PVariable(name)
    }

  lazy val parens: PackratParser[PExpression] =
    "(" ~> expression <~ ")"
}

// object TermParser extends StdTokenParsers {
//   type Tokens = StdLexical
//
//   val lexical = new StdLexical
//   lexical.delimiters ++= Seq("[", "]", "=")
//
//   def parse[T](str: String): Term = {
//     val tokens = new lexical.Scanner(str)
//     phrase(term[T])(tokens) match {
//       case Success(parsed, _) => parsed
//       case NoSuccess(err, _) => throw new Exception("err")
//     }
//   }
//
//   def term[T]: Parser[Term] =
//     assignment[T] | expr[T] | static
//
//   def assignment[T]: Parser[Assignment[T]] = ???
//
//   def expr[T]: Parser[Expr[T]] = ???
//
//   def static: Parser[Static] = ???
//
// }