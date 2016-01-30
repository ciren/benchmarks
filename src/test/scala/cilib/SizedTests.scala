package cilib
package benchmarks

import org.scalacheck._

import scalaz.Scalaz._

import Sized._

object SizedTests extends Properties("Sized") {

  property("sized1") = {
    toSized1[List,Int](List()) === None &&
    toSized1(List(1)) === Some(1) &&
    toSized1(List(1,2)) === Some(1)
  }

  property("sized2") = {
    toSized2[List,Int](List()) === None &&
    toSized2(List(1)) === None &&
    toSized2(List(1,2)) === Some((1,2)) &&
    toSized2(List(1,2,3)) === Some((1,2))
  }

  property("sized3") = {
    toSized3[List,Int](List()) === None &&
    toSized3(List(1)) === None &&
    toSized3(List(1,2)) === None &&
    toSized3(List(1,2,3)) === Some((1,2,3)) &&
    toSized3(List(1,2,3,4)) === Some((1,2,3))
  }

  property("sized4") = {
    toSized4[List,Int](List()) === None &&
    toSized4(List(1)) === None &&
    toSized4(List(1,2)) === None &&
    toSized4(List(1,2,3)) === None &&
    toSized4(List(1,2,3,4)) === Some((1,2,3,4)) &&
    toSized4(List(1,2,3,4,5)) === Some((1,2,3,4))
  }

  property("sized5") = {
    toSized5[List,Int](List()) === None &&
    toSized5(List(1)) === None &&
    toSized5(List(1,2)) === None &&
    toSized5(List(1,2,3)) === None &&
    toSized5(List(1,2,3,4)) === None &&
    toSized5(List(1,2,3,4,5)) === Some((1,2,3,4,5)) &&
    toSized5(List(1,2,3,4,5,6)) === Some((1,2,3,4,5))
  }

  property("sized6") = {
    toSized6[List,Int](List()) === None &&
    toSized6(List(1)) === None &&
    toSized6(List(1,2)) === None &&
    toSized6(List(1,2,3)) === None &&
    toSized6(List(1,2,3,4)) === None &&
    toSized6(List(1,2,3,4,5)) === None &&
    toSized6(List(1,2,3,4,5,6)) === Some((1,2,3,4,5,6)) &&
    toSized6(List(1,2,3,4,5,6,7)) === Some((1,2,3,4,5,6))
  }
}
