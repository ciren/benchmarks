package cilib
package benchmarks

import scalaz.Foldable
import scalaz.syntax.foldable._

import spire.algebra.{Field,Ring}
import spire.implicits._

object Ops {

  implicit class RangeOps[A](x: Range) {
    def sumr(f: Int => A)(implicit ev: Ring[A]): A = x.toList.foldLeft(ev.zero)((b: A, a: Int) => b + f(a))
  }

  implicit class ListOps[A,B](x: List[A]) {
    def suml(f: A => B)(implicit ev: Ring[B]): B = x.foldLeft(ev.zero)((b: B, a: A) => b + f(a))
    def productl(f: A => B)(implicit ev: Field[B]): B = x.foldLeft(ev.one)((b: B, a: A) => b * f(a))
    def pairs = x.sliding(2).toList.map { case Seq(x1, x2) => (x1, x2) }
  }

  implicit class FoldableOps[F[_]: Foldable, A, B](x: F[A]) {
    def sum(f: A => B)(implicit ev: Ring[B]): B = x.foldLeft(ev.zero)((b: B, a: A) => b + f(a))
    def product(f: A => B)(implicit ev: Field[B]): B = x.foldLeft(ev.one)((b: B, a: A) => b * f(a))
  }

}
