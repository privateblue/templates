package language

import scala.io.Source
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(source: String): String = {
    val result = for {
      terms <- TermParser.parse(source)
      rendered <- TermEvaluator.compile(terms, ExpressionEvaluator.EmptyContext)
    } yield rendered
    result match {
      case Right(r) => r
      case Left(err) => err.msg
    }
  }

  def repl(context: ExpressionEvaluator.Context = ExpressionEvaluator.EmptyContext, count: Int = 0): Unit = {
    val str = scala.io.StdIn.readLine("> ")
    if (str != "quit") for {
      parsed <- ExpressionParser.parse(str)
      evaluated <- ExpressionEvaluator.eval(parsed, context)
      result = evaluated match {
        case (Bool(v), _) => v.toString
        case (Number(v), _) => v.toString
        case (Text(v), _) => v
      }
      name = s"res$count"
      _ = println(s"$name := $parsed ( = $result)")
      nctx = ExpressionEvaluator.put(name, parsed, context)
    } yield repl(nctx, count + 1)
  }

  def compileFile(filename: String): Unit = {
    val source = Source.fromFile(filename).getLines.mkString("\n")
    println(source)
    println
    val result = for {
      terms <- TermParser.parse(source)
      rendered <- TermEvaluator.compile(terms, ExpressionEvaluator.EmptyContext)
    } yield rendered
    result match {
      case Right(r) => println(r)
      case Left(err) => println("An error occured")
    }
  }
}