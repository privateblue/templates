package templates

import cats.implicits._

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    sources.map(render(EmptyContext))
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
