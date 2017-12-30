package benchmarks

import shapeless._
import shapeless.ops.nat._

import dimension.syntax._

package object dimension {
  object implicits extends DimensionSyntax

  val _30  = Nat.apply(30)
  val _50  = Nat.apply(50)
  val _100 = Nat.apply(100)

  type _1   = nat._1
  type _2   = nat._2
  type _3   = nat._3
  type _4   = nat._4
  type _5   = nat._5
  type _6   = nat._6
  type _8   = nat._8
  type _10  = nat._10
  type _16  = nat._16
  type _30  = _30.N
  type _50  = _50.N
  type _100 = _100.N

  type Dimension[N<:Nat,A] = Sized[IndexedSeq[A],N]
  type Dimension1[A]       = Sized[IndexedSeq[A],_1]
  type Dimension2[A]       = Sized[IndexedSeq[A],_2]
  type Dimension3[A]       = Sized[IndexedSeq[A],_3]
  type Dimension4[A]       = Sized[IndexedSeq[A],_4]
  type Dimension5[A]       = Sized[IndexedSeq[A],_5]
  type Dimension6[A]       = Sized[IndexedSeq[A],_6]
  type Dimension8[A]       = Sized[IndexedSeq[A],_8]
  type Dimension10[A]      = Sized[IndexedSeq[A],_10]
  type Dimension30[A]      = Sized[IndexedSeq[A],_30]
  type Dimension50[A]      = Sized[IndexedSeq[A],_50]

  type GTEq1[N<:Nat]   = GTEq[N,_1]
  type GTEq2[N<:Nat]   = GTEq[N,_2]
  type HasHead[N<:Nat] = LT[_0,N]
}
