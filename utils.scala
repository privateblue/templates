package templates

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    val templates = sources.toList.zipWithIndex.map { case (s, i) => s"template$i" -> Right(Source(s)) }
    val start = (js.Array[String](), Context(ExpressionEvaluator.EmptyContext, templates.toMap, List.empty))
    templates.foldLeft(start) { case ((results, ctx), (name, _)) =>
      TemplateCompiler.compile(name, ctx) match {
        case Right((r, ctx1)) => (results :+ r, ctx1)
        case Left((err, ctx1)) => (results :+ err.msg, ctx1)
      }
    }._1
  }
}