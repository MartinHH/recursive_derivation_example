import scala.collection.mutable.AbstractIterable
import scala.deriving.*
import scala.compiletime.{error, erasedValue, summonFrom, summonInline}

inline def summonInstances[T, Elems <: Tuple]: List[Eq[?]] =
  inline erasedValue[Elems] match
    case _: (elem *: elems) =>
      deriveOrSummon[T, elem] :: summonInstances[T, elems]
    case _: EmptyTuple => Nil

inline def deriveOrSummon[T, Elem]: Eq[Elem] =
  inline erasedValue[Elem] match
    case _: T => deriveRec[T, Elem]
    case _    => summonOrDerive[Elem]

inline def deriveRec[T, Elem]: Eq[Elem] =
  inline erasedValue[T] match
    case _: Elem => error("infinite recursive derivation")
    case _ =>
      Eq.derived[Elem](using
        summonInline[Mirror.Of[Elem]]
      ) // recursive derivation

// prefers existing givens so that in case of a recursion
// somewhere in a nested product, the given result from
// Eq.derived is used instead of re-deriving
inline def summonOrDerive[T]: Eq[T] =
  summonFrom {
    case eq: Eq[T]       => eq
    case m: Mirror.Of[T] => Eq.derived(using m)
  }

trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  def check(x: Any, y: Any, elem: Eq[?]): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterable[T](p: T): Iterable[Any] = new AbstractIterable[Any]:
    def iterator: Iterator[Any] = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(x, y, elems(ordx))

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[?]]): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean =
        iterable(x).lazyZip(iterable(y)).lazyZip(elems).forall(check)

  inline def derived[T](using m: Mirror.Of[T]): Eq[T] =
    lazy val elemInstances = summonInstances[T, m.MirroredElemTypes]
    // make result available as given within the scope of derivation:
    given eq: Eq[T] = inline m match
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    eq
end Eq

case class Tail[+T](ts: Lst[T])

enum Lst[+T] derives Eq:
  case Cns(t: T, tail: Tail[T])
  case Nl

extension [T](t: T) def ::(ts: Lst[T]): Lst[T] = Lst.Cns(t, Tail(ts))

@main def test(): Unit =
  import Lst.*
  val eqoi = summon[Eq[Lst[Int]]]
  assert(eqoi.eqv(23 :: 47 :: Nl, 23 :: 47 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, 7 :: Nl))
  assert(!eqoi.eqv(23 :: Nl, Nl))
