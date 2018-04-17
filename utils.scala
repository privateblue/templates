package templates

import scala.io.Source
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(source: String): String = {
    val rendered = for {
      parsed <- TemplateParser.parse(source)
      compiled <- TemplateCompiler.compile(parsed)
    } yield compiled
    rendered match {
      case Right(r) => r
      case Left(err) => err.msg
    }
  }
}