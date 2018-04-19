package templates

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    val context = sources.toList.zipWithIndex.map { case (s, i) => s"template$i" -> Val(Text(s)) }
    sources.map(render(context))
  }

  def render(context: ExpressionEvaluator.Context)(source: String): String = {
    val rendered = for {
      parsed <- TemplateParser.parse(source)
      compiled <- TemplateCompiler.compile(parsed, context)
    } yield compiled
    rendered.fold(_.msg, _._1)
  }
}
