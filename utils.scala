package templates

import cats.implicits._

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    val context = sources.toList.zipWithIndex.map { case (s, i) => s"template$i" -> Val(Text(s)) }
    sources.map(render(context))
  }

  def render(context: Context)(source: String): String = {
    val rendered = for {
      _ <- set(context)
      parsed <- lift(TemplateParser.parse(source))
      compiled <- TemplateEvaluator.eval(parsed)
    } yield compiled
    rendered.runEmptyA.fold(_.msg, identity)
  }
}
