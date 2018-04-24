package templates

import org.parboiled2._
import org.parboiled2.Parser.DeliveryScheme.Either

import cats._
import cats.implicits._

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

sealed trait Block
case class Term(expr: Expression) extends Block
case class Static(content: String) extends Block

case class Template(blocks: List[Block])

object TemplateEvaluator {
  def eval(template: Template): Contexted[String] =
    template.blocks.foldMapM {
      case Term(expr) => ExpressionEvaluator.eval(expr).map(ValuePrinter.print)
      case Static(content) => result(content)
    }

  def fold[T: Monoid](template: Template)(v: Variable => T)(a: Assignment => T): T =
    template.blocks.foldMap {
      case Term(expr) => ExpressionEvaluator.fold(expr)(v)(a)
      case Static(content) => Monoid[T].empty
    }
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
      ExpressionParser.parse(str).fold(_ => MISMATCH, stmt => rule(push(Term(stmt))))
    }
  }

  def static: Rule1[Static] = rule {
    capture(oneOrMore(noneOf("[]"))) ~> Static
  }
}
