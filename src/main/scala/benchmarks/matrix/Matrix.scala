package benchmarks
package matrix

import dimension._
import dimension.implicits._
import scalaz.Scalaz._
import shapeless._
import shapeless.ops.nat._
import spire.algebra._
import spire.implicits._
import spire.math.Interval

import cilib._

object Matrix {

  def wrap[M <: Nat, N <: Nat, A](cols: IndexedSeq[A]*): Matrix[M, N, A] =
    Sized.wrap[IndexedSeq[Dimension[N, A]], M](cols.toVector.map { c =>
      Sized.wrap[IndexedSeq[A], N](c)
    })

  // identity matrix
  def eye[N <: Nat: GTEq1: ToInt, A: Ring]: Matrix[N, N, A] = {
    val size = implicitly[ToInt[N]].apply
    val A    = implicitly[Ring[A]]
    val elements =
      for {
        c <- 0 until size
      } yield for {
        r <- 0 until size
        v = if (c == r) A.one else A.zero
      } yield v
    Matrix.wrap[N, N, A](elements: _*)
  }

  // alpha matrix
  def alpha[N <: Nat: GTEq1: ToInt, A: Field: NRoot](α: Double): Matrix[N, N, A] = {
    val size = implicitly[ToInt[N]].apply
    val A    = implicitly[Field[A]]
    val elements =
      for {
        c <- 0 until size
      } yield for {
        r <- 0 until size
        v = if (c == r) A.fromDouble(α) ** (0.5 * (c / (size - 1.0)))
        else A fromDouble 0.0
      } yield v
    Matrix.wrap[N, N, A](elements: _*)
  }

  def random[N <: Nat: GTEq1: ToInt](i: Interval[Double]): RVar[Matrix[N, N, Double]] = {
    val size = implicitly[ToInt[N]].apply
    val elements =
      for {
        c <- 0 until size
      } yield for {
        r <- 0 until size
      } yield Dist uniform i
    val transformed = elements.toVector.traverse(_.toVector.sequence)
    transformed.map(els => Matrix.wrap[N, N, Double](els: _*))
  }

  def rotation[N <: Nat: GTEq1: ToInt]: RVar[Matrix[N, N, Double]] = {
    def helper(
      remaining: Vector[Dimension[N, Double]],
      us: Vector[Dimension[N, Double]]
    ): Vector[Dimension[N, Double]] = remaining match {
      case v +: vs =>
        val u = us.headOption.map(_ => v - us.map(_ project v).reduce(_ + _)).getOrElse(v)
        helper(vs, us :+ u)
      case _ => us
    }

    for {
      m <- Matrix.random[N](Interval(-1.0, 1.0))
    } yield Sized.wrap(helper(m.toVector, Vector())) map (_.normalized)
  }
}
