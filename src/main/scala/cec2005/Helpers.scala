package benchmarks
package cec2005

import scala.io.Source.fromResource

import scalaz.Scalaz._

import shapeless._
import shapeless.ops.nat._

import dimension._
import matrix._

object Helpers {

  def fbiasFromResource(fNumber: Int) =
    fromResource("cec2005/fbias_data.txt")
      .mkString
      .trim
      .split("\\s+")
      .toList.toNel
      .flatMap { _.index(fNumber - 1) }
      .get.toDouble

  def shiftFromLine[N<:Nat](line: String)(implicit ev: ToInt[N]): Dimension[N,Double] = {
    val shift = line
      .trim
      .split("\\s+")
      .map { _.toDouble }
      .take(ev.apply)
      .toVector
    Sized.wrap(shift)
  }

  def shiftFromResource[N<:Nat:ToInt](resource: String): Dimension[N,Double] =
    shiftFromLine(fromResource(s"cec2005/$resource").getLines.toList.head)

  def shiftsFromResource[N<:Nat:ToInt](resource: String): List[Dimension[N,Double]] =
    fromResource(s"cec2005/$resource")
      .getLines
      .toList
      .map(shiftFromLine[N])

  def shiftFromResourceF[N<:Nat:ToInt](resource: String, f: List[String] => String) =
    (f andThen shiftFromLine[N])(fromResource(s"cec2005/$resource").getLines.toList)

  def matrixFromLines[N<:Nat](lines: List[String])(implicit ev: ToInt[N]) = {
    val dim = ev.apply
    val elements =
      lines
        .take(dim)
        .flatMap {
          _
            .trim
            .split("\\s+")
            .take(dim)
            .map(_.toDouble)
        }
        .grouped(dim)
        .map(_.toVector)
        .toVector

    Matrix.wrap[N,N,Double](elements: _*)
  }

  def matrixFromResource[N<:Nat:ToInt](resource: String) =
    matrixFromLines(fromResource(s"cec2005/$resource").getLines.toList)

  def matrixFromResourceF[N<:Nat:ToInt](resource: String, f: List[String] => List[String]) =
    (f andThen matrixFromLines[N])(fromResource(s"cec2005/$resource").getLines.toList)

  def matrixFromResourceTail[N<:Nat:ToInt](resource: String) =
    matrixFromResourceF(resource, _.tail)

  def matrix10FromResource[N<:Nat:ToInt](resource: String): Dimension10[Matrix[N,N,Double]] = {
    val dim = implicitly[ToInt[N]].apply
    val lines = fromResource(s"cec2005/$resource")
      .getLines.toList

    val matrices = lines
      .grouped(dim)
      .map { group =>
        val elements = group
          .mkString
          .trim
          .split("\\s+")
          .map { _.toDouble }
          .grouped(dim)
          .map(_.toVector)
          .toVector
        Matrix.wrap[N,N,Double](elements: _*)
      }
    Sized.wrap(matrices.toVector)
  }

}
