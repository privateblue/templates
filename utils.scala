package templates

import cats.implicits._

import scala.scalajs.js
import scala.scalajs.js.annotation._

object UI {
  @JSExportTopLevel("render")
  def render(sources: js.Array[String]): js.Array[String] = {
    val parsed = sources.map(TemplateParser.parse)
    val context = parsed.toList.zipWithIndex.collect {
      case (Right(t), i) =>
        val name = s"template$i"
        val module = Val(Module(t))
        val used = TemplateEvaluator.fold(t)(v => Set(v.name))(_ => Set.empty[String])
        val defined = TemplateEvaluator.fold(t)(v => Set.empty[String])(a => Set(a.name))
        val unbound = used &~ defined
        if (unbound.isEmpty) name -> module
        else name -> Val(Function(unbound.toList, Render(module)))
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
