package templates

import cats.implicits._

import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(source: String): String = {
    val rendered = for {
      parsed <- lift(TemplateParser.parse(source))
      compiled <- TemplateCompiler.compile(parsed)
    } yield compiled
    rendered.runEmptyA.fold(_.msg, identity)
  }
}