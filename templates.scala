package templates

import org.parboiled2._

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import scala.util.{Success, Failure}

sealed trait Statement
case class Assignment(name: String, expression: Expression) extends Statement
case class Return(expression: Expression) extends Statement

sealed trait Block
case class Term(statement: Statement) extends Block
case class Static(content: String) extends Block

case class Template(blocks: List[Block])

object StatementEvaluator {
  type Context = ExpressionEvaluator.Context

  def eval(stmt: Statement, ctx: Context): Result[(String, Context)] =
    stmt match {
      case Assignment(name, expr) =>
        val updated = ExpressionEvaluator.put(name, expr, ctx)
        Right(("", updated))

      case Return(expr) =>
        ExpressionEvaluator.eval(expr, ctx).flatMap {
          case (Bool(v), ctx1) => Right((v.toString, ctx1))
          case (Number(v), ctx1) => Right((v.toString, ctx1))
          case (Text(v), ctx1) => Right((v, ctx1))
        }
    }
}

object TemplateCompiler {
  type Context = ExpressionEvaluator.Context
  val EmptyContext = ExpressionEvaluator.EmptyContext

  def compile(template: Template, context: Context = EmptyContext): Result[String] = {
    val start: Result[(String, Context)] = Right(("", context))
    val rendering = template.blocks.foldLeft(start) {
      case (acc, Term(stmt)) => acc.flatMap(r => StatementEvaluator.eval(stmt, r._2).map(e => (r._1 + e._1, e._2)))
      case (acc, Static(content)) => acc.map(r => (r._1 + content, r._2))
    }
    rendering.map(_._1)
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
    expression ^^ {
      case expr => Return(expr)
    }
}

object TemplateParser {
  def parse(str: String): Result[Template] = {
    val parser = new TemplateParser(str)
    parser.template.run() match {
      case Success(template) => Right(template)
      case Failure(err) => Left(SyntaxError(err.getMessage))
    }
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
      StatementParser.parse(str) match {
        case Right(stmt) => push(Term(stmt))
        case Left(err) => MISMATCH
      }
    }
  }

  def static: Rule1[Static] = rule {
    capture(oneOrMore(noneOf("[]"))) ~> Static
  }
}
