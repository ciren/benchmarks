package benchmarks

import org.scalacheck._
import spire.math.Interval
import spire.implicits._

object Generators {

  val intervalGen = for {
    a <- Gen.choose(-100.0, -1.0) suchThat { _ != 0.0 }
    b <- Gen.choose(1.0, 100.0) suchThat { _ != 0.0  }
  } yield Interval(a, b)

  implicit val arbInterval: Arbitrary[Interval[Double]] = Arbitrary(intervalGen)
}
