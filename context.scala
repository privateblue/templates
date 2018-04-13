import shapeless._

trait Context { self =>
  type L <: HList

  val bindings: L

  val search = SearchIn(bindings)

  def lookup[T](name: String): Option[Expression[T]] =
    search.find[T](name)

  def store[T](name: String, expr: Expression[T]): Context =
    new Context {
      type L = Binding[T] :: self.L
      val bindings = Binding(name, expr) :: self.bindings
    }
}

object Context {
  val empty = new Context {
    type L = HNil
    val bindings = HNil
  }
}

case class Binding[T](key: String, value: Expression[T])

trait Search[In, Q] {
  def find(key: String)(in: In): Option[Expression[Q]]
}

trait LowPrioritySearch {
  implicit def hlistish[In, L <: HList, Q](
    implicit gen: Generic.Aux[In, L], s: Search[L, Q]
  ): Search[In, Q] = new Search[In, Q] {
    def find(key: String)(in: In) = s.find(key)(gen to in)
  }
}

object Search extends LowPrioritySearch {
  def apply[In, Q](implicit search: Search[In, Q]) = search

  implicit def binding[Q]: Search[Binding[Q], Q] =
    new Search[Binding[Q], Q] {
      def find(key: String)(in: Binding[Q]) =
        if (in.key == key) Some(in.value) else None
    }

  implicit def list[In, Q](implicit s: Search[In, Q]): Search[List[In], Q] =
    new Search[List[In], Q] {
      def find(key: String)(in: List[In]) = in.flatMap(s.find(key)).headOption
    }

  implicit def nil[Q]: Search[HNil, Q] =
    new Search[HNil, Q] {
      def find(key: String)(in: HNil) = None
    }

  implicit def cons[H, T <: HList, Q](
    implicit hs: Search[H, Q] = null, ts: Search[T, Q]
  ): Search[H :: T, Q] =
    new Search[H :: T, Q] {
      def find(key: String)(in: H :: T) =
        Option(hs).flatMap(s => s.find(key)(in.head)) orElse ts.find(key)(in.tail)
    }
}

case class SearchIn[In](in: In) {
  def find[Q](key: String)(implicit search: Search[In, Q]) = search.find(key)(in)
}