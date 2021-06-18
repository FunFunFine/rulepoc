package hmap
import Tuple.*
trait Tup
case class TCons[H, T <: Tup](h: H, t: T) extends Tup
case object TNil extends Tup

// Type level natural numbers -------------------------------------------------

sealed trait Nat
sealed trait Succ[P <: Nat] extends Nat
sealed trait Zero extends Nat

// Accessor type class to compute the N'th element of an Tuple L --------------

trait At[L <: Tuple, N <: Nat, Out] {
  def apply(l: L): Out
}

object At {
  implicit def caseZero[H, T <: Tuple]: At[H *: T, Zero, H] =
    new At[H *: T, Zero, H] {
      def apply(l: H *: T): H = {
        val (h *: _) = l
        h
      }
    }

  implicit def caseN[H, T <: Tuple, N <: Nat, O](implicit
      a: At[T, N, O]
  ): At[H *: T, Succ[N], O] =
    new At[H *: T, Succ[N], O] {
      def apply(l: H *: T): O = {
        val (_ *: t) = l
        a(t)
      }
    }
}

// An HMap is an Tuple with HEntry elements. We are reusing Tuple for it's nice syntax

final case class HEntry[K, V](value: V)

// Accessor type class to compute the element of type K in a HMap L -----------

trait PhantomGet[K, M <: Tuple, I <: Nat] // extends PhantomAny

object PhantomGet {
  implicit def getHead[K, V, T <: Tuple]
      : PhantomGet[K, HEntry[K, V] *: T, Zero] = null

  implicit def getTail[K, H, T <: Tuple, I <: Nat](implicit
      t: PhantomGet[K, T, I]
  ): PhantomGet[K, H *: T, Succ[I]] = null
}

// Syntax ---------------------------------------------------------------------

object syntax {
  object hmap {
    implicit class HmapGet[M <: Tuple](m: M) {
      def get[K, V, I <: Nat](k: K)(implicit
          g: PhantomGet[k.type, M, I],
          a: At[M, I, HEntry[k.type, V]]
      ): V = a(m).value
    }

    def --[K, V](key: K, value: V) = HEntry[key.type, V](value)
  }
}

object Test {

  def main: Unit = {
    import syntax.hmap.*

    val map1 =
      (
        HEntry["name", String]("foo"),
        HEntry["genre", Boolean](true),
        HEntry["moneyz", Int](123),
        HEntry["cat", String]("bar")
      )

    assert(map1.get("name") == "foo")
    assert(map1.get("genre") == true)
    assert(map1.get("moneyz") == 123)
    assert(map1.get("cat") == "bar")

    println(map1)

  }
}
