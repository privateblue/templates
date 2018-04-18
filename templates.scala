package templates

import org.parboiled2._

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.PackratParsers

import scala.util.{Success, Failure}

sealed trait Statement
case class Include(name: String) extends Statement
case class Assignment(name: String, expression: Expression) extends Statement
case class Return(expression: Expression) extends Statement

sealed trait Block
case class Term(statement: Statement) extends Block
case class Static(content: String) extends Block

sealed trait Template
case class Source(src: String) extends Template
case class AST(blocks: List[Block]) extends Template

case class Context(
  variables: ExpressionEvaluator.Context,
  templates: Map[String, Result[Template]],
  includePath: List[String]
) {
  def putVar(name: String, expr: Expression): Context =
    Context(ExpressionEvaluator.put(name, expr, variables), templates, includePath)

  def replaceVars(ctx: ExpressionEvaluator.Context): Context =
    Context(ctx, templates, includePath)

  def findTemplate(name: String): Result[Template] =
    templates.get(name)
      .toRight(Undefined(s"Template $name not found"))
      .flatMap(identity)

  def putTemplate(name: String, template: AST): Context =
    Context(variables, templates + (name -> Right(template)), includePath)

  def replaceTemplates(ts: Map[String, Result[Template]]): Context =
    Context(variables, ts, includePath)

  def pushInclude(name: String): Result[Context] =
    if (includePath.contains(name)) {
      val path = includePath.reverse.mkString("", " -> ", s" -> $name")
      Left(CyclicReference(s"Template $name includes itself: $path"))
    } else Right(Context(variables, templates, name :: includePath))
}

object StatementEvaluator {
  def eval(stmt: Statement, ctx: Context): Either[(LanguageError, Context), (String, Context)] =
    stmt match {
      case Include(name) =>
        TemplateCompiler.compile(name, ctx)

      case Assignment(name, expr) =>
        val updated = ctx.putVar(name, expr)
        Right(("", updated))

      case Return(expr) =>
        ExpressionEvaluator.eval(expr, ctx.variables).flatMap {
          case (Bool(v), ctx1) => Right((v.toString, ctx.replaceVars(ctx1)))
          case (Number(v), ctx1) => Right((v.toString, ctx.replaceVars(ctx1)))
          case (Text(v), ctx1) => Right((v, ctx.replaceVars(ctx1)))
        }.errorWith(ctx)
    }
}

object TemplateCompiler {
  def compile(name: String, context: Context): Either[(LanguageError, Context), (String, Context)] =
    for {
      found <- findParseMemoize(name, context).errorWith(context)
      (parsed, ctx1) = found
      ctx2 = context.replaceTemplates(ctx1.templates)
      ctx3 <- ctx2.pushInclude(name).errorWith(ctx2)
      result <- compileAst(parsed, ctx3).errorWith(ctx2)
      (compiled, ctx4) = result
    } yield (compiled, context.replaceTemplates(ctx4.templates))

  def findParseMemoize(name: String, context: Context): Result[(AST, Context)] =
    for {
      found <- context.findTemplate(name)
      parsed <- found match {
        case Source(src) => TemplateParser.parse(src)
        case ast @ AST(_) => Right(ast)
      }
    } yield (parsed, context.putTemplate(name, parsed))

  def compileAst(template: AST, context: Context): Result[(String, Context)] = {
    val start: Result[(String, Context)] = Right(("", context))
    template.blocks.foldLeft(start) {
      case (results, Term(stmt)) =>
        results.flatMap { case (previous, ctx1) =>
          StatementEvaluator.eval(stmt, ctx1).ignoreWith.map(r => (previous + r._1, r._2))
        }
      case (results, Static(content)) =>
        results.map { case (previous, ctx1) => (previous + content, ctx1) }
    }
  }
}

object StatementParser extends ExpressionParser {
  lexical.delimiters ++= Seq("=")
  lexical.reserved += ("include")

  def parse(str: String): Result[Statement] = {
    val tokens = new lexical.Scanner(str)
    phrase(statement)(tokens) match {
      case Success(parsed, _) => Right(parsed)
      case NoSuccess(err, _) => Left(SyntaxError(err))
    }
  }

  lazy val statement: PackratParser[Statement] =
    include | assignment | `return`

  lazy val include: PackratParser[Include] =
    "include" ~ ident ^^ {
      case _ ~ name => Include(name)
    }

  lazy val assignment: PackratParser[Assignment] =
    ident ~ "=" ~ expression ^^ {
      case name ~ "=" ~ expr => Assignment(name, expr)
    }

  lazy val `return`: PackratParser[Return] =
    expression ^^ Return.apply
}

object TemplateParser {
  def parse(src: String): Result[AST] = {
    val parser = new TemplateParser(src)
    parser.template.run() match {
      case Success(template) => Right(template)
      case Failure(err) => Left(SyntaxError(err.getMessage))
    }
  }
}

class TemplateParser(val input: ParserInput) extends Parser {
  def template: Rule1[AST] = rule {
    zeroOrMore(block) ~ EOI ~> { (blocks: Seq[Block]) =>
      AST(blocks.toList)
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
