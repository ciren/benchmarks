package benchmarks
package matrix
package syntax

import scala.language.implicitConversions

import _root_.scala.Predef.{ any2stringadd => _, _ }
import benchmarks.implicits._
import dimension._
import shapeless._
import spire.algebra.{ Eq, Field, Ring }
import spire.implicits._

// simple column major matrix
final class MatrixOps[C <: Nat, R <: Nat, A](m: Matrix[C, R, A]) {

  // scalar-matrix multiplication
  def *(s: A)(implicit A: Field[A]): Matrix[C, R, A] =
    m map { col =>
      col map (_ * s)
    }

  // scalar-matrix division
  def /(s: A)(implicit A: Field[A]): Matrix[C, R, A] =
    m map { col =>
      col map (_ / s)
    }

  // matrix vector multiplication
  def *(v: Dimension[C, A])(implicit ev: Ring[A]): Dimension[R, A] =
    m.t map { _ innerProduct v }

  // matrix matrix multiplication
  def |*|[P <: Nat](other: Matrix[P, C, A])(implicit ev: Ring[A]): Matrix[P, R, A] =
    (m.t map { row =>
      other map { _ innerProduct row }
    }).t

  // matrix transpose
  lazy val t: Matrix[R, C, A] = Matrix.wrap(m.transpose: _*)

}

trait MatrixSyntax {
  implicit def ToMatrixOps[M <: Nat, N <: Nat, A](v: Matrix[M, N, A]): MatrixOps[M, N, A] = new MatrixOps[M, N, A](v)
  implicit def ToMatrixEqOps[M <: Nat, N <: Nat, A: Eq]: Eq[Matrix[M, N, A]] = new Eq[Matrix[M, N, A]] {
    def eqv(x: Matrix[M, N, A], y: Matrix[M, N, A]): Boolean =
      (x zip y) forAll { case (xi, yi) => xi === yi }
  }
}
