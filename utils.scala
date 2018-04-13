package language

import scala.io.Source

object ExpressionRepl {
  def main(args: Array[String]): Unit = eval()

  def eval(context: ExpressionEvaluator.Context = ExpressionEvaluator.EmptyContext, count: Int = 0): Unit = {
    val str = scala.io.StdIn.readLine("> ")
    if (str != "quit") for {
      parsed <- ExpressionParser.parse(str)
      evaluated <- ExpressionEvaluator.eval(parsed, context)
      result = evaluated match {
        case (Bool(v), _) => v.toString
        case (Number(v), _) => v.toString
        case (Text(v), _) => v
      }
      name = s"res$count"
      _ = println(s"$name := $parsed ( = $result)")
      nctx = ExpressionEvaluator.put(name, parsed, context)
    } yield eval(nctx, count + 1)
  }
}

object FileProcessor {
  def main(args: Array[String]): Unit = args.toList match {
    case filename :: _ => process(filename)
    case _ => println("File name missing")
  }

  def process(filename: String): Unit = {
    val source = Source.fromFile(filename).getLines.mkString("\n")
    println(source)
    println
    val result = for {
      terms <- TermParser.parse(source)
      rendered <- TermEvaluator.compile(terms, ExpressionEvaluator.EmptyContext)
    } yield rendered
    result match {
      case Right(r: String) => println(r)
      case Left(err) => println("An error occured")
    }
  }
}