package cilib
package benchmarks

import org.scalacheck._
import org.scalacheck.Gen

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.list._
import scalaz.syntax.apply._
import scalaz.{NonEmptyList,IList}

import Sized._

object Generators {

  def gen1(l: Double, u: Double): Gen[Double] =
    Gen.choose(l, u)

  def gen2(l: Double, u: Double): Gen[(Double, Double)] =
    (Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple2.apply }

  def gen2D(d1: (Double, Double), d2: (Double, Double)) =
    (Gen.choose(d1._1, d1._2) |@| Gen.choose(d2._1, d2._2)) { Tuple2.apply }

  def gen3(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple3.apply }

  def gen3D(d1: (Double, Double), d2: (Double, Double), d3: (Double, Double)) =
    (Gen.choose(d1._1, d1._2) |@| Gen.choose(d2._1, d2._2) |@| Gen.choose(d3._1, d3._2)) { Tuple3.apply }

  def gen4(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l,u)) { Tuple4.apply }

  def gen5(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple5.apply }

  def gen6(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple6.apply }

  def gen10(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@|
     Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple10.apply }

  def gen1And(l: Double, u: Double): Gen[Sized1And[List,Double]] =
    (gen1(l, u) |@| Gen.containerOf[List, Double](gen1(l, u))) { (a, b) => Sized1And(a, b) }

  def gen2And(l: Double, u: Double) =
    (gen2(l, u) |@| Gen.containerOf[List, Double](gen1(l, u))) { (a, b) => Sized2And(a._1, a._2, b) }

  def genNEL(l: Double, u: Double): Gen[NonEmptyList[Double]] =
    gen1And(l, u).map(x => NonEmptyList.nel(x.head, IList.fromFoldable(x.tail)))

  def genConst(v: Double) = genNEL(v, v)

}
