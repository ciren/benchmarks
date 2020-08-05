package benchmarks
package syntax

import scala.language.implicitConversions

import benchmarks.dimension._
import scalaz.NonEmptyList
import scalaz.Scalaz._
import shapeless._
import shapeless.ops.nat._

import cilib._

final class EvalOps[N <: Nat: ToInt](f: Dimension[N, Double] => Double) {

  type D[A] = Dimension[N, A]

  def unconstrained: Eval[D, Double] = {
    implicit val input: Input[D] = new Input[D] {
      def toInput[B](a: NonEmptyList[B]): D[B] = {
        val dim = implicitly[ToInt[N]].apply
        if (dim != a.size) sys.error("Input vector dimension is not the same as the benchmark function")
        else Sized.wrap[IndexedSeq[B], N](a.toVector)
      }
    }

    val g: D[Double] => Double = f

    Eval unconstrained g
  }
}

trait EvalSyntax {
  implicit def ToEvalOps[N <: Nat: ToInt](f: Dimension[N, Double] => Double): EvalOps[N] = new EvalOps[N](f)
}
