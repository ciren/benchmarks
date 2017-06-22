package benchmarks

import shapeless._

import dimension._
import matrix.syntax._

package object matrix {
  // only square matrices for now
  type Matrix[N<:Nat,A] = Dimension[N,Dimension[N,A]]

  object implicits extends MatrixSyntax
}
