package benchmarks
package syntax

import org.scalacheck._
import org.scalacheck.Prop._

import spire.implicits._

import benchmarks.implicits._

object SyntaxTests extends Properties("Syntax Tests") {

  val doubleGen = Gen.choose(-1000.0, 1000.0)
  val gen = Gen.listOf(doubleGen)

  property("List mapSum") = forAll { (a: List[Double]) =>
    a.mapSum(_ * 1.0) === a.sum &&
    a.mapSum(_ * 2.0) === a.map(_ * 2.0).sum &&
    a.mapSum(_ * 0.0) === 0.0
  }

  property("List mapProduct") = forAll(gen) { a =>

    a.mapProduct(_ * 1.0) === a.product &&
    a.mapProduct(_ * 2.0) === a.map(_ * 2.0).product &&
    a.mapProduct(ai => 1.0) === 1.0
  }
}
