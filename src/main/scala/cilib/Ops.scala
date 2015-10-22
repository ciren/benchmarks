package cilib
package benchmarks

import scalaz.{Foldable,Monoid}
import scalaz.syntax.foldable1._
import scalaz.std.list._

import spire.algebra.Field
import spire.implicits._

object Ops {

  implicit class RangeOps[A: Monoid](x: Range) {
    def sumr(f: Int => A) = x.toList.foldMap(f)
  }

  implicit class ListOps[A,B](x: List[A]) {
    def suml(f: A => B)(implicit ev: Monoid[B]): B = x.foldMap(f)
    def productl(f: A => B)(implicit ev: Field[B]): B = x.foldLeft(ev.one)((b: B, a: A) => b * f(a))
    def pairs = x.sliding(2).toList.map { case Seq(x1, x2) => (x1, x2) }
  }

  implicit class FoldableOps[F[_]: Foldable, A, B](x: F[A]) {
    def sum(f: A => B)(implicit ev: Monoid[B]): B = x.foldMap(f)
    def product(f: A => B)(implicit ev: Field[B]): B = x.foldLeft(ev.one)((b: B, a: A) => b * f(a))
  }

}
