package templates

import org.parboiled2._
import org.parboiled2.Parser.DeliveryScheme.Either

import cats.implicits._

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

sealed trait Statement
case class Assignment(name: String, expression: Expression) extends Statement
case class Return(expression: Expression) extends Statement

sealed trait Block
case class Term(statement: Statement) extends Block
case class Static(content: String) extends Block

case class Template(blocks: List[Block])

object StatementEvaluator {
  def eval(stmt: Statement): Contexted[String] =
    stmt match {
      case Assignment(name, expr) =>
        push(name, expr).map(_ => "")

      case Return(expr) =>
        ExpressionEvaluator.eval(expr).flatMap {
          case Bool(v) => result(v.toString)
          case Number(v) => result(v.toString)
          case Text(v) => result(v)
        }
    }
}

object TemplateEvaluator {
  def eval(template: Template): Contexted[String] =
    template.blocks.foldMapM {
      case Term(stmt) => StatementEvaluator.eval(stmt)
      case Static(content) => result(content)
    }
}

object StatementParser extends ExpressionParser {
  lexical.delimiters ++= Seq("=")

  def parse(str: String): Result[Statement] = {
    val tokens = new lexical.Scanner(str)
    phrase(statement)(tokens) match {
      case Success(parsed, _) => Right(parsed)
      case NoSuccess(err, _) => Left(SyntaxError(err))
    }
  }

  lazy val statement: PackratParser[Statement] =
    assignment | `return`

  lazy val assignment: PackratParser[Assignment] =
    ident ~ "=" ~ expression ^^ {
      case name ~ "=" ~ expr => Assignment(name, expr)
    }

  lazy val `return`: PackratParser[Return] =
    expression ^^ Return.apply
}

object TemplateParser {
  def parse(str: String): Result[Template] = {
    val parser = new TemplateParser(str)
    val run = parser.template.run()
    run.leftMap(err => SyntaxError(err.getMessage))
  }
}

class TemplateParser(val input: ParserInput) extends Parser {
  def template: Rule1[Template] = rule {
    zeroOrMore(block) ~ EOI ~> { (blocks: Seq[Block]) =>
      Template(blocks.toList)
    }
  }

  def block: Rule1[Block] = rule {
    term | static
  }

  def term: Rule1[Term] = rule {
    "[" ~ capture(oneOrMore(noneOf("[]"))) ~ "]" ~> { (str: String) =>
      StatementParser.parse(str).fold(_ => MISMATCH, stmt => rule(push(Term(stmt))))
    }
  }

  def static: Rule1[Static] = rule {
    capture(oneOrMore(noneOf("[]"))) ~> Static
  }
}
