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
}