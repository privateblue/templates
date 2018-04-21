package templates

import cats.implicits._

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    val parsed = sources.map(TemplateParser.parse)
    val context = parsed.toList.zipWithIndex.collect {
      case (Right(t), i) => s"template$i" -> Val(Module(t))
    }
    parsed.map(_.fold(_.msg, render(context)))
  }

  def render(context: Context)(template: Template): String = {
    val rendered = for {
      _ <- set(context)
      compiled <- TemplateEvaluator.eval(template)
    } yield compiled
    rendered.runEmptyA.fold(_.msg, identity)
  }
}
