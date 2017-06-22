package benchmarks
package cec2005

import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose

import dimension.Generators._
import dimension._

object Generators {
  def genCECSized[A:Choose](l: A, u: A):
    Gen[(Dimension[_2,A],Dimension[_10,A],Dimension[_30,A],Dimension[_50,A])] =
      for {
        a <- gen2(l, u)
        b <- gen10(l, u)
        c <- gen30(l, u)
        d <- gen50(l, u)
      } yield (a, b, c, d)
}
