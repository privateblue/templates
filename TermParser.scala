package templates

import org.parboiled2._

import cats.instances.list._
import cats.instances.either._
import cats.syntax.traverse._

import scala.util.{Success, Failure}

sealed trait PTerm
case class PAssignment(name: String, expression: String) extends PTerm
case class PExpr(expression: String) extends PTerm
case class PStatic(content: String) extends PTerm

object TermParser {
  def parse(str: String): Result[List[Term]] = {
    val parser = new TermParser(str)
    parser.terms.run() match {
      case Success(terms) => parseExpressions(terms)
      case Failure(err) => Left(SyntaxError(err.getMessage))
    }
  }

  def parseExpressions(pterms: Seq[PTerm]): Result[List[Term]] =
    pterms.toList.traverse {
      case PAssignment(name, expr) =>
        ExpressionParser.parse(expr).map(Assignment(name, _))

      case PExpr(expr) =>
        ExpressionParser.parse(expr).map(Expr.apply)

      case PStatic(content) =>
        Right[LanguageError, Term](Static(content))
    }
}

class TermParser(val input: ParserInput) extends Parser {
  def terms: Rule1[Seq[PTerm]] = rule {
    zeroOrMore(term) ~ EOI
  }

  def term: Rule1[PTerm] = rule {
    assignment | expr | static
  }

  def assignment: Rule1[PAssignment] = rule {
    "[" ~ ws ~ capture(ident) ~ ws ~ "=" ~ ws ~ capture(expression) ~ ws ~ "]" ~> PAssignment
  }

  def expr: Rule1[PExpr] = rule {
    "[" ~ ws ~ capture(expression) ~ ws ~ "]" ~> PExpr
  }

  def static: Rule1[PStatic] = rule {
    capture(oneOrMore(noneOf("[]"))) ~> PStatic
  }

  def ident: Rule0 = rule {
    oneOrMore(CharPredicate.Alpha)
  }

  def expression: Rule0 = rule {
    oneOrMore(noneOf("[]="))
  }

  def ws: Rule0 = rule {
    zeroOrMore(" " | "\n" | "\t" | "\r")
  }
}