package benchmarks
package matrix
package syntax

import _root_.scala.Predef.{any2stringadd => _, _}

import scala.language.implicitConversions

import shapeless._
import spire.algebra.{Ring}

import benchmarks.implicits._
import dimension._

// simple column major matrix
final class MatrixOps[N<:Nat,A](m: Matrix[N,A]) {

  // matrix vector multiplication
  def *(v: Dimension[N,A])(implicit ev: Ring[A]): Dimension[N,A] =
    m.map { _ innerProduct v }

  // matrix transpose
  lazy val t: Matrix[N,A] = Matrix.wrap(m.transpose)
}

trait MatrixSyntax {
  implicit def ToMatrixOps[N<:Nat,A](v: Matrix[N,A]): MatrixOps[N,A] = new MatrixOps[N,A](v)
}
