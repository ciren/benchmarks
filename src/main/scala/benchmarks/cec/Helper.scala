package benchmarks
package cec

// import scala.io.Source.fromResource

import benchmarks.Benchmarks.{mapSum}
// import benchmarks.dimension._
// import benchmarks.implicits._
import benchmarks.matrix._
// import scalaz.Scalaz._
// import shapeless._
// import shapeless.ops.nat._

import cilib._

// case class Helper(prefix: String) {

//   def fbiasFromResource(fNumber: Int): Double =
//     fromResource(s"${prefix}/fbias_data.txt").mkString.trim
//       .split("\\s+")
//       .toList
//       .toNel
//       .flatMap { _.index(fNumber - 1) }
//       .get
//       .toDouble

//   def shiftFromLine[N <: Nat: ToInt](line: String): Dimension[N, Double] = {
//     val shift = line.trim
//       .split("\\s+")
//       .map(_.toDouble)
//       .take(implicitly[ToInt[N]].apply)
//       .toVector
//     Sized.wrap(shift)
//   }

//   def shiftFromResource[N <: Nat: ToInt](resource: String): Dimension[N, Double] =
//     shiftFromLine(fromResource(s"${prefix}/$resource").getLines.toList.head)

//   def shiftsFromResource[N <: Nat: ToInt](resource: String): List[Dimension[N, Double]] =
//     fromResource(s"${prefix}/$resource").getLines.toList
//       .map(shiftFromLine[N])

//   def shiftFromResourceF[N <: Nat: ToInt](resource: String, f: List[String] => String) =
//     (f andThen shiftFromLine[N])(fromResource(s"${prefix}/$resource").getLines.toList)

//   def matrixFromLines[N <: Nat: ToInt](lines: List[String]): Matrix[N, N, Double] = {
//     val dim = implicitly[ToInt[N]].apply
//     val elements =
//       lines
//         .take(dim)
//         .flatMap {
//           _.trim
//             .split("\\s+")
//             .take(dim)
//             .map(_.toDouble)
//         }
//         .grouped(dim)
//         .map(_.toVector)
//         .toVector

//     Matrix.wrap[N, N, Double](elements: _*)
//   }

//   def matrixFromResource[N <: Nat: ToInt](resource: String): Matrix[N, N, Double] =
//     matrixFromLines(fromResource(s"${prefix}/$resource").getLines.toList)

//   def matricesFromResource[M <: Nat: ToInt, N <: Nat: ToInt](resource: String): Dimension[M, Matrix[N, N, Double]] = {
//     val num = implicitly[ToInt[M]].apply
//     val dim = implicitly[ToInt[N]].apply
//     val matrices = fromResource(s"${prefix}/$resource").getLines
//       .grouped(dim)
//       .toList
//       .map(lines => matrixFromLines[N](lines.toList).t)

//     Sized.wrap(matrices.toVector.take(num))
//   }

//   def matrixFromResourceF[N <: Nat: ToInt](resource: String, f: List[String] => List[String]): Matrix[N, N, Double] =
//     (f andThen matrixFromLines[N])(fromResource(s"${prefix}/$resource").getLines.toList)

//   def matrixFromResourceTail[N <: Nat: ToInt](resource: String) =
//     matrixFromResourceF(resource, _.tail)

//   def matrix10FromResource[N <: Nat: ToInt](resource: String): Dimension10[Matrix[N, N, Double]] = {
//     val dim   = implicitly[ToInt[N]].apply
//     val lines = fromResource(s"${prefix}/$resource").getLines.toList

//     val matrices = lines
//       .grouped(dim)
//       .map { group =>
//         val elements = group.mkString.trim
//           .split("\\s+")
//           .map { _.toDouble }
//           .grouped(dim)
//           .map(_.toVector)
//           .toVector
//         Matrix.wrap[N, N, Double](elements: _*)
//       }
//     Sized.wrap(matrices.toVector)
//   }

// }

object Helper {

  def zip[A, B](a: NonEmptyVector[A], b: NonEmptyVector[B]): NonEmptyVector[(A,B)] =
    a.zipWith(b)(Tuple2.apply)

  def shift(x: NonEmptyVector[Double], o: NonEmptyVector[Double]): NonEmptyVector[Double] =
    x.zipWith(o)(_ - _)


  def hybrid(
    b: NonEmptyVector[Double],
    o: NonEmptyVector[NonEmptyVector[Double]],
    m: NonEmptyVector[Matrix[Double]],
    f: NonEmptyVector[NonEmptyVector[Double] => Double],
    λ: NonEmptyVector[Double],
    σ: NonEmptyVector[Double]
  ): NonEmptyVector[Double] => Double = {
    val C = 2000.0
    x => {
      val D = x.length
      val fmax = (zip(zip(f, λ), m)) map {
        case ((fi, λi), mi) =>
          val temp: NonEmptyVector[Double] = NonEmptyVector.fromIterableOption(Vector.fill(D)(5.0 / λi)).get
          val point                      = mi.rotate(temp)
          math.abs(fi(point))
      }

      val zipped = (zip(zip(zip(zip(zip(zip(o, m), f), λ), σ), b), fmax)) map {
        case ((((((oi, mi), fi), λi), σi), bi), fmaxi) =>
          val zi = mi.rotate(shift(x,oi).map(_ / λi))
          (oi, mi, fi, λi, σi, bi, fmaxi, zi)
      }
      val weights = zipped map {
        case (oi, _, _, _, σi, _, _, zi) =>
          val denom = mapSum(zip(x, oi) ) { case (xk, oik) => (xk - oik) * (xk - oik) }
          math.exp(-denom / (2.0 * zi.length * σi * σi))
      }

      val maxWeight = weights.toChunk.max
      val w1mMaxPow = 1.0 - math.pow(maxWeight, 10)
      val adjustedWeights = weights map { wi =>
        if (wi != maxWeight) wi * w1mMaxPow
        else wi
      }
      val wSum =  mapSum(adjustedWeights) (xi => xi)
      // normalize the weights
      val normWeights = adjustedWeights map { _ / wSum }

      mapSum(zip(zipped, normWeights)) {
        case ((_, _, fi, _, _, bi, fmaxi, zi), wi) =>
          wi * ((C * fi(zi) / fmaxi) + bi)
      }
    }
  }

//   def hybridR[M <: Nat, N <: Nat: ToInt, A: Field: Ordering: Signed: Trig](
//     b: Dimension[M, Double],
//     o: Dimension[M, Dimension[N, A]],
//     m: Dimension[M, Matrix[N, N, A]],
//     f: Dimension[M, Dimension[N, A] => RVar[A]],
//     λ: Dimension[M, Double],
//     σ: Dimension[M, Double]
//   ): Dimension[N, A] => RVar[A] = {
//     val C = 2000.0
//     val D = implicitly[ToInt[N]].apply
//     val fmax: RVar[Dimension[M, A]] = (f zip λ zip m) traverse {
//       case ((fi, λi), mi) =>
//         val temp: Dimension[N, Double] = Sized.wrap(Vector.fill(D)(5.0 / λi))
//         val point                      = temp.map(implicitly[Field[A]].fromDouble) rotate mi
//         fi(point).map(abs(_))
//     }
//     x => {
//       val zipped = fmax map { fMax =>
//         (o zip m zip f zip λ zip σ zip b zip fMax) map {
//           case ((((((oi, mi), fi), λi), σi), bi), fmaxi) =>
//             val zi = x.shift(oi).map(_ / λi).rotate(mi)
//             (oi, mi, fi, λi, σi, bi, fmaxi, zi)
//         }
//       }
//       val weights = zipped map {
//         _.map {
//           case (oi, _, _, _, σi, _, _, zi) =>
//             val denom = (x zip oi) mapSum { case (xk, oik) => (xk - oik) ** 2 }
//             exp(-denom / (2.0 * zi.size * σi * σi))
//         }
//       }

//       val normWeights = for {
//         ws        <- weights
//         maxWeight = ws.max
//         w1mMaxPow = 1.0 - (maxWeight ** 10)
//         adjusted  = ws.map(wi => if (wi != maxWeight) wi * w1mMaxPow else wi)
//         wSum      = adjusted mapSum (xi => xi)
//       } yield adjusted map { _ / wSum }

//       for {
//         z  <- zipped
//         nw <- normWeights
//         rs <- (z zip nw) traverse {
//                case ((_, _, fi, _, _, bi, fmaxi, zi), wi) =>
//                  fi(zi) map { fiz =>
//                    wi * (C * fiz / fmaxi + bi)
//                  }
//              }
//       } yield rs.mapSum(xi => xi)
//     }
//   }

}
