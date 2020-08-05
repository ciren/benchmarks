package benchmarks
package dimension
package syntax

import scala.language.implicitConversions

import _root_.scala.Predef.{ any2stringadd => _, _ }
import matrix._
import matrix.implicits._
import scalaz._, Scalaz._
import shapeless._
import spire.algebra.{ Eq, Field, NRoot, Ring }
import spire.implicits._
import spire.math.Interval

import cilib._

final class DimensionOps[N <: Nat, A](x: Dimension[N, A]) {
  lazy val size: Int = x.length

  def zipWithIndex: Dimension[N, (A, Int)] =
    Sized.wrap[IndexedSeq[(A, Int)], N](x.zipWithIndex)

  def zip[B](other: Dimension[N, B]): Dimension[N, (A, B)] =
    Sized.wrap(x.zip(other))

  def mapSum[B: Ring](f: A => B) =
    x.foldLeft(implicitly[Ring[B]].zero)((b, a) => b + f(a))

  def forAll(f: A => Boolean) =
    x.foldLeft(true)((b, a) => b && f(a))

  def mapProduct[B: Ring](f: A => B) =
    x.foldLeft(implicitly[Ring[B]].one)((b, a) => b * f(a))

  def traverse[G[_]: Applicative, B](f: A => G[B]): G[Dimension[N, B]] =
    x.toVector.traverse(f).map(Sized.wrap[IndexedSeq[B], N])

  def shift(other: Dimension[N, A])(implicit ev: Ring[A]): Dimension[N, A] =
    Sized.wrap[IndexedSeq[A], N]((x zip other) map { case (xi, oi) => xi - oi })

  def -(other: Dimension[N, A])(implicit ev: Ring[A]): Dimension[N, A] = shift(other)
  def +(other: Dimension[N, A])(implicit ev: Ring[A]): Dimension[N, A] = shift(other.map(-_))

  def rotate[R <: Nat](m: Matrix[N, R, A])(implicit ev: Ring[A]): Dimension[R, A] = m * x

  def innerProduct(other: Dimension[N, A])(implicit ev: Ring[A]): A =
    x.zip(other)
      .map { case (xi, oi) => xi * oi }
      .foldLeft(ev.zero)(_ + _)

  def randomize(interval: Interval[Double])(implicit A: Field[A]): RVar[Dimension[N, A]] = {
    def f(a: A) = Dist.uniform(interval) map A.fromDouble
    traverse(f)
  }

  lazy val t: Matrix[N, nat._1, A] = x map { xi =>
    Sized(xi)
  }

  def project(other: Dimension[N, A])(implicit ev: Field[A]): Dimension[N, A] = {
    val factor = innerProduct(other) / innerProduct(x)
    x map { _ * factor }
  }

  def magnitude(implicit A: Field[A], R: NRoot[A]): A = mapSum(_ ** 2).sqrt

  def normalized(implicit A: Field[A], R: NRoot[A]): Dimension[N, A] = {
    val mag = magnitude
    x map { _ / mag }
  }

}

final class DimensionGTEq2Ops[N <: Nat: GTEq2, A](x: Dimension[N, A]) {
  // type signature is incorrect
  // should return Dimension[Prev[N],(A,A)]
  lazy val pairs: Dimension[N, (A, A)] =
    Sized.wrap(x.sliding(2).toVector.map { case Seq(x1, x2) => (x1, x2) })
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
  implicit def ToDimensionOps[N <: Nat, A](v: Dimension[N, A]): DimensionOps[N, A] = new DimensionOps[N, A](v)
  implicit def ToDimensionGTEq2Ops[N <: Nat: GTEq2, A](v: Dimension[N, A]): DimensionGTEq2Ops[N, A] =
    new DimensionGTEq2Ops[N, A](v)
  implicit def ToDimension2Ops[A](v: Dimension2[A]): Dimension2Ops[A] = new Dimension2Ops[A](v)
  implicit def ToDimension3Ops[A](v: Dimension3[A]): Dimension3Ops[A] = new Dimension3Ops[A](v)
  implicit def ToDimension4Ops[A](v: Dimension4[A]): Dimension4Ops[A] = new Dimension4Ops[A](v)
  implicit def ToDimension5Ops[A](v: Dimension5[A]): Dimension5Ops[A] = new Dimension5Ops[A](v)
  implicit def ToDimension6Ops[A](v: Dimension6[A]): Dimension6Ops[A] = new Dimension6Ops[A](v)
  implicit def ToDimensionEqOps[N <: Nat, A: Eq]: Eq[Dimension[N, A]] = new Eq[Dimension[N, A]] {
    def eqv(x: Dimension[N, A], y: Dimension[N, A]): Boolean =
      (x zip y) forAll { case (xi, yi) => xi === yi }
  }
}
