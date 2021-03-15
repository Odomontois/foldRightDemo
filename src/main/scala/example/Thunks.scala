package example
import scala.annotation.tailrec
import scala.collection.IterableFactoryDefaults
import scala.collection.LinearSeq
import scala.collection.LinearSeqOps
import scala.collection.SeqFactory
import scala.collection.mutable.Builder
import scala.collection.mutable.ImmutableBuilder

import cats.Eval
import cats.Foldable
import cats.syntax.foldable._
sealed trait LoosyList[+A]
    extends LinearSeq[A]
    with LinearSeqOps[A, LoosyList, LoosyList[A]]
    with IterableFactoryDefaults[A, LoosyList] {
  override def iterableFactory = LoosyList
  override def isEmpty = this eq Nah
  override def head: A = throw new NoSuchElementException
  override def tail: LoosyList[A] = throw new NoSuchElementException
}

case object Nah extends LoosyList[Nothing]
case class Chons[+A](leader: () => A, followers: () => LoosyList[A])
    extends LoosyList[A] {
  override def head: A = leader()
  override def tail = followers()
}

object LoosyList extends SeqFactory[LoosyList] {
  def empty[A]: LoosyList[A] = Nah

  def chons[A](a: => A, t: => LoosyList[A]): LoosyList[A] =
    Chons(() => a, () => t)

  def iterate[A](a: A)(f: A => A): LoosyList[A] = chons(a, iterate(f(a))(f))

  implicit object foldable extends Foldable[LoosyList] {
    @tailrec override def foldLeft[A, B](fa: LoosyList[A], b: B)(
        f: (B, A) => B
    ): B = fa match {
      case Nah                      => b
      case Chons(leader, followers) => foldLeft(followers(), f(b, leader()))(f)
    }

    override def foldRight[A, B](fa: LoosyList[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] = fa match {
      case Nah => lb
      case Chons(leader, followers) =>
        Eval.defer(f(leader(), foldRight(followers(), lb)(f)))
    }
  }

  def from[A](source: IterableOnce[A]) =
    source.iterator.foldRight[LoosyList[A]](empty)(chons(_, _))

  def newBuilder[A]: Builder[A, LoosyList[A]] =
    new ImmutableBuilder[A, LoosyList[A]](Nah) {
      def addOne(elem: A) = {
        elems = chons(elem, elems)
        this
      }
      override def result(): LoosyList[A] = elems.reverse
    }
}

object Thunks {

  def xs = LoosyList.iterate(0L)(_ + 1)

  def sumUntil[T[_]: Foldable](xs: => T[Long], limit: Long): Long =
    xs.foldRight[Long => Eval[Long]](Eval.now(x => Eval.now(x)))((x, t) =>
      if (x > limit) Eval.always(x => Eval.now(x))
      else Eval.always(s => t.flatMap(f => f(s + x)))
    ).value(0)
      .value

  def main(args: Array[String]): Unit = {
    println(sumUntil(xs, args.lift(0).fold(1_000_000L)(_.toLong)))
  }
}
