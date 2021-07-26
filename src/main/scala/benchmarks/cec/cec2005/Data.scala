package benchmarks.cec.cec2005

import scala.io.Source.fromResource

import zio.prelude.NonEmptyList
import zio._

import benchmarks.matrix._

import atto._, Atto._

object Data {
  val nonWhitespace: Parser[String] = Atto.many1(Atto.oneOf(".-+eE0123456789")).map(_.toList.mkString)
  val doubleParser: Parser[Double] = nonWhitespace.map(_.toDouble)
  val listOfDoubles: Parser[List[Double]] = many(Atto.skipWhitespace ~> doubleParser)

  def parseResource[A](resource: String, parser: Parser[A]): Task[Iterator[A]] =
    ZIO.effect {
      fromResource(resource).getLines().map(line => {
        parser.parseOnly(line) match {
          case ParseResult.Fail(_, _, message) => throw new java.io.IOException(message)
          case ParseResult.Done(_, result) => result
          case ParseResult.Partial(_) => throw new java.io.IOException("Parsing of resource failed: Obtained a partial parse result")
        }
      })
    }

  def parseResourceMatrix(resource: String): Matrix[Double] = {
    val matrix =
      parseResource(resource, listOfDoubles)
        .map(_.map(_.toVector).toVector)

    Runtime.default.unsafeRunTask(matrix)
  }

  def parseResourceVector(resource: String): Vector[Double] = {
    val vector =
      parseResource(resource, listOfDoubles)
        .map(_.map(_.toVector).toVector.flatten)

    Runtime.default.unsafeRunTask(vector)
  }

  def parseResourceNonEmptyList(resource: String): NonEmptyList[Double] = {
    val list : Task[NonEmptyList[Double]] =
      parseResource(resource, listOfDoubles)
        .map(iter => NonEmptyList.fromIterableOption(iter.flatten.toList).get)

    Runtime.default.unsafeRunTask(list)
  }


  lazy val sphere_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/sphere_func_data.txt")

  lazy val schwefel_102_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/schwefel_102_data.txt")

  lazy val high_cond_elliptic_rot_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/high_cond_elliptic_rot_data.txt")

  lazy val elliptic_M_D2: Matrix[Double] =
    parseResourceMatrix("cec2005/elliptic_M_D2.txt")

  lazy val elliptic_M_D10: Matrix[Double] =
    parseResourceMatrix("cec2005/elliptic_M_D10.txt")

  lazy val elliptic_M_D30: Matrix[Double] =
    parseResourceMatrix("cec2005/elliptic_M_D30.txt")

  lazy val elliptic_M_D50: Matrix[Double] =
    parseResourceMatrix("cec2005/elliptic_M_D50.txt")

  lazy val schwefel_206_data: Matrix[Double] =
    parseResourceMatrix("cec2005/schwefel_206_data.txt")

  lazy val rosenbrock_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/rosenbrock_func_data.txt")

  lazy val griewank_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/griewank_func_data.txt")

  lazy val griewank_M_D2: Matrix[Double] =
    parseResourceMatrix("cec2005/griewank_M_D2.txt")

  lazy val griewank_M_D10: Matrix[Double] =
    parseResourceMatrix("cec2005/griewank_M_D10.txt")

  lazy val griewank_M_D30: Matrix[Double] =
    parseResourceMatrix("cec2005/griewank_M_D30.txt")

  lazy val griewank_M_D50: Matrix[Double] =
    parseResourceMatrix("cec2005/griewank_M_D50.txt")

}
