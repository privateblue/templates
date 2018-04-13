package language

sealed trait Value
case class Bool(underlying: Boolean) extends Value
case class Number(underlying: Int) extends Value
case class Text(underlying: String) extends Value

sealed trait Expression
case class Variable(name: String) extends Expression
case class Val(underlying: Value) extends Expression
case class If(condition: Expression, yes: Expression, no: Expression) extends Expression
case class Add(left: Expression, right: Expression) extends Expression
case class Mult(left: Expression, right: Expression) extends Expression
case class And(left: Expression, right: Expression) extends Expression
case class Not(expr: Expression) extends Expression

sealed trait LanguageError
case object SyntaxError extends LanguageError
case object Undefined extends LanguageError
case object TypeError extends LanguageError

object ExpressionEvaluator {
  type Context = List[(String, Expression)]

  val EmptyContext: Context = List.empty

  def put(name: String, expr: Expression, ctx: Context): Context =
    (name, expr) :: ctx

  def find(
    name: String,
    ctx: Context,
    prefix: Context = EmptyContext
  ): Result[(Context, Context)] =
    ctx match {
      case Nil => Left(Undefined)
      case (key, expr) :: rest if key == name => Right((prefix, ctx))
      case head :: rest => find(name, rest, prefix :+ head)
    }

  def eval(expr: Expression, ctx: Context): Result[(Value, Context)] =
    expr match {
      case Variable(name) =>
        for {
          split <- find(name, ctx)
          (rest, (_, expr) :: ctx1) = split
          evaluated <- eval(expr, ctx1)
          (value, ctx2) = evaluated
          binding = (name, Val(value))
          ctx3 = binding :: ctx2
        } yield (value, rest ++ ctx3)

      case Val(value) =>
        Right((value, ctx))

      case If(condition, yes, no) =>
        for {
          condEval <- eval(condition, ctx)
          (cond, ctx1) = condEval
          c <- cond match {
            case Bool(b) => Right(b)
            case _ => Left(TypeError)
          }
          result <- if (c) eval(yes, ctx1) else eval(no, ctx1)
        } yield result

      case Add(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Number(n), Number(m)) => Right((Number(n + m), ctx2))
            case _ => Left(TypeError)
          }
        } yield result

      case Mult(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Number(n), Number(m)) => Right((Number(n * m), ctx2))
            case _ => Left(TypeError)
          }
        } yield result

      case And(left, right) =>
        for {
          leftEval <- eval(left, ctx)
          (l, ctx1) = leftEval
          rightEval <- eval(right, ctx1)
          (r, ctx2) = rightEval
          result <- (l, r) match {
            case (Bool(n), Bool(m)) => Right((Bool(n && m), ctx2))
            case _ => Left(TypeError)
          }
        } yield result

      case Not(expr) =>
        eval(expr, ctx).flatMap {
          case (Bool(b), ctx1) => Right((Bool(!b), ctx1))
          case _ => Left(TypeError)
        }
    }
}

sealed trait Term
case class Assignment(name: String, expression: Expression) extends Term
case class Expr(expression: Expression) extends Term
case class Static(content: String) extends Term

object TermEvaluator {
  type Context = ExpressionEvaluator.Context

  def eval(term: Term, ctx: Context): Result[(String, Context)] =
    term match {
      case Assignment(name, expr) =>
        val updated = ExpressionEvaluator.put(name, expr, ctx)
        Right(("", updated))

      case Expr(expr) =>
        ExpressionEvaluator.eval(expr, ctx).flatMap {
          case (Bool(v), ctx1) => Right((v.toString, ctx1))
          case (Number(v), ctx1) => Right((v.toString, ctx1))
          case (Text(v), ctx1) => Right((v, ctx1))
        }

      case Static(content) =>
        Right((content, ctx))
    }

  def compile(terms: List[Term], context: Context): Result[String] = {
    val start: Result[(String, Context)] = Right(("", context))
    val rendering = terms.foldLeft(start) {
      case (acc, term) => acc.flatMap(r => eval(term, r._2).map(e => (r._1 + e._1, e._2)))
    }
    rendering.map(_._1)
  }
}