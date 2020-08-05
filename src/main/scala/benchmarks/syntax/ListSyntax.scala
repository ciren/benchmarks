package benchmarks
package syntax

import scala.language.implicitConversions

import spire.algebra.{ Field, Ring }
import spire.implicits._

final class ListOps[A](x: List[A]) {
  def mapSum[B](f: A => B)(implicit ev: Ring[B]): B =
    x.foldLeft(ev.zero)((b: B, a: A) => b + f(a))

  def mapProduct[B](f: A => B)(implicit ev: Field[B]): B =
    x.foldLeft(ev.one)((b: B, a: A) => b * f(a))

  def pairs =
    x.sliding(2).toList.map { case Seq(x1, x2) => (x1, x2) }
}

trait ListSyntax {
  implicit def ToListOps[A](v: List[A]): ListOps[A] = new ListOps[A](v)
}
