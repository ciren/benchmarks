package benchmarks
package dimension
package syntax

import _root_.scala.Predef.{any2stringadd => _, _}
import scala.language.implicitConversions
import scala.reflect.ClassTag

import scalaz._
import Scalaz._

import shapeless._
import spire.algebra.{Field,Ring}
import spire.implicits._

import matrix._
import matrix.implicits._

final class DimensionOps[N<:Nat,A](x: Dimension[N,A]) {
  def size: Int = x.length

  def zipWithIndex: Dimension[N,(A,Int)] =
    Sized.wrap[IndexedSeq[(A,Int)],N](x.zipWithIndex)

  def zip[B](other: Dimension[N,B]): Dimension[N,(A,B)] =
    Sized.wrap(x.zip(other))

  def mapSum[B](f: A => B)(implicit ev: Ring[B]) =
    x.foldLeft(ev.zero)((b, a) => b + f(a))

  def mapProduct[B](f: A => B)(implicit ev: Field[B]) =
    x.foldLeft(ev.one)((b, a) => b * f(a))

  def traverse[G[_]:Applicative,B](f: A => G[B]): G[Dimension[N,B]] =
    x.toVector.traverse(f).map(Sized.wrap[IndexedSeq[B],N])

  def pairs(implicit gt: GTEq2[N]): Dimension[N,(A,A)] =
    Sized.wrap(x.sliding(2).toVector.map { case Seq(x1, x2) => (x1, x2) })

  def shift(other: Dimension[N,A])(implicit ev: Ring[A]): Dimension[N,A] =
    Sized.wrap[IndexedSeq[A],N]((x zip other) map { case (xi, oi) => xi - oi })

  def rotate(m: Matrix[N,A])(implicit ev: Ring[A]): Dimension[N,A] = m * x

  def innerProduct(other: Dimension[N,A])(implicit ev: Ring[A]): A =
    x.zip(other)
      .map { case (xi, oi) => xi * oi }
      .foldLeft(ev.zero)(_ + _)
}

final class Dimension2Ops[A](x: Dimension2[A]) {
  lazy val tuple = (x.head, x.last)
}
final class Dimension3Ops[A](x: Dimension3[A]) {
  lazy val tuple = (x.head, x(1), x.last)
}
final class Dimension4Ops[A](x: Dimension4[A]) {
  lazy val tuple = (x.head, x(1), x(2), x.last)
}
final class Dimension5Ops[A](x: Dimension5[A]) {
  lazy val tuple = (x.head, x(1), x(2), x(3), x.last)
}
final class Dimension6Ops[A](x: Dimension6[A]) {
  lazy val tuple = (x.head, x(1), x(2), x(3), x(4), x.last)
}

trait DimensionSyntax {
  implicit def ToDimensionOps[N<:Nat,A](v: Dimension[N,A]): DimensionOps[N,A] = new DimensionOps[N,A](v)
  implicit def ToDimension2Ops[A](v: Dimension2[A]): Dimension2Ops[A] = new Dimension2Ops[A](v)
  implicit def ToDimension3Ops[A](v: Dimension3[A]): Dimension3Ops[A] = new Dimension3Ops[A](v)
  implicit def ToDimension4Ops[A](v: Dimension4[A]): Dimension4Ops[A] = new Dimension4Ops[A](v)
  implicit def ToDimension5Ops[A](v: Dimension5[A]): Dimension5Ops[A] = new Dimension5Ops[A](v)
  implicit def ToDimension6Ops[A](v: Dimension6[A]): Dimension6Ops[A] = new Dimension6Ops[A](v)
}
