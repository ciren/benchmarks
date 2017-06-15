package benchmarks
package matrix

import shapeless._
import shapeless.ops.nat._

import dimension._

object Matrix {

  def wrap[N<:Nat,A](cols: IndexedSeq[IndexedSeq[A]]): Matrix[N,A] =
    Sized.wrap[IndexedSeq[Dimension[N,A]],N](cols.map { c =>
      Sized.wrap[IndexedSeq[A],N](c)
    })

  // identity matrix
  def eye[N<:Nat:GTEq1](implicit ev: ToInt[N]): Matrix[N,Double] = {
    val elements =
      for {
        c <- 0 until ev.apply
      } yield for {
        r <- 0 until ev.apply
        v  = if (c == r) 1.0 else 0.0
      } yield v
    Matrix.wrap[N,Double](elements)
  }
}
