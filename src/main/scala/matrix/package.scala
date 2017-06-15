package benchmarks

import shapeless._

import dimension._
import matrix.syntax._

package object matrix {
  type Matrix[C<:Nat,R<:Nat,A] = Dimension[C,Dimension[R,A]]

  object implicits extends MatrixSyntax
}
