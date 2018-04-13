sealed trait Expression[+T]
case class Variable[T](name: String) extends Expression[T]
case class Value[T](underlying: T) extends Expression[T]
case class If[T](condition: Expression[Boolean], yes: Expression[T], no: Expression[T]) extends Expression[T]
case class Add(left: Expression[Int], right: Expression[Int]) extends Expression[Int]
case class Mult(left: Expression[Int], right: Expression[Int]) extends Expression[Int]
case class And(left: Expression[Boolean], right: Expression[Boolean]) extends Expression[Boolean]
case class Not(expr: Expression[Boolean]) extends Expression[Boolean]

object ExpressionEvaluator {
  def eval[T](expr: Expression[T], ctx: Context): T =
    expr match {
      case Variable(name) =>
        val expr = ctx.lookup(name).getOrElse(throw new Exception("Variable not defined"))
        eval(expr, ctx)

      case Value(underlying) =>
        underlying

      case If(condition, yes, no) =>
        if (eval(condition, ctx)) eval(yes, ctx) else eval(no, ctx)

      case Add(left, right) =>
        eval(left, ctx) + eval(right, ctx)

      case Mult(left, right) =>
        eval(left, ctx) * eval(right, ctx)

      case And(left, right) =>
        eval(left, ctx) && eval(right, ctx)

      case Not(expr) =>
        !eval(expr, ctx)
    }
}

object repl {
  def main(args: Array[String]): Unit = eval()

  def eval(context: Context = Context.empty, count: Int = 0): Unit = {
    val str = scala.io.StdIn.readLine("> ")
    if (str != "quit") {
      val parsed = ExpressionParser.parse(str)
      val evaluated = ExpressionEvaluator.eval(parsed, context)
      val name = s"res$count"
      println(s"$name := $parsed ( = $evaluated)")
      val nctx = context.store(name, parsed)
      println(nctx)
      eval(nctx, count + 1)
    }
  }
}

// sealed trait Term
// case class Assignment[T](name: String, expression: Expression[T]) extends Term
// case class Expr[T](expression: Expression[T]) extends Term
// case class Static(content: String) extends Term
//
// object Term {
//   def eval(term: Term, ctx: Context): (String, Context) =
//     term match {
//       case Assignment(name, expression) =>
//         val value = Expression.eval(expression, ctx)
//         val updated = ctx.store(name, value)
//         ("", updated)
//
//       case Expr(expression) =>
//         val value = Expression.eval(expression, ctx)
//         (value.toString, ctx)
//
//       case Static(content) =>
//         (content, ctx)
//     }
//
//   def compile(terms: List[Term], context: Context): String = {
//     val start = ("", context)
//     val (rendered, _) = terms.foldLeft(start) {
//       case ((result, ctx), term) =>
//         val (evaluated, updated) = eval(term, ctx)
//         (result + evaluated, updated)
//     }
//     rendered
//   }
// }