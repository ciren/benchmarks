package benchmarks.cec.cec2005

import scala.io.Source.fromResource

import zio.prelude.NonEmptyList
import zio._

import benchmarks.matrix._

import atto._, Atto._
import _root_.scala.io.BufferedSource
import cilib.NonEmptyVector

sealed abstract class DataSource {
  def limit(n: Int): DataSource
}

final case class Schwefel213Data(matrixA: Matrix[Double], matrixB: Matrix[Double], alpha: Vector[Double]) extends DataSource {
  def limit(n: Int) =
    Schwefel213Data(matrixA.mapRow(_.take(n)), matrixB.mapRow(_.take(n)), alpha.take(n))
}

final case class Schwefel206Data(shift: Vector[Double], matrixA: Matrix[Double]) extends DataSource {
  def limit(n: Int) =
    Schwefel206Data(shift.take(n), matrixA.mapRow(_.take(n)))
}

final case class HybridData(shiftVectors: NonEmptyVector[NonEmptyVector[Double]]) extends DataSource {
  def limit(n: Int) =
    HybridData(shiftVectors.map(vector => NonEmptyVector.fromChunk(vector.toChunk.take(n)).get))
}


object Data {
  val nonWhitespace: Parser[String] = Atto.many1(Atto.oneOf(".-+eE0123456789")).map(_.toList.mkString)
  val doubleParser = Atto.skipWhitespace ~> nonWhitespace.map(_.toDouble) <~ Atto.skipWhitespace

  def fromResourceEffect[A](resourcePath: String, parser: Parser[A]) =
    ZIO.bracket(
      acquire = ZIO.effect(fromResource(resourcePath)),
      release = (r: BufferedSource) => Task.succeed(r.close()),
      use = (resource: BufferedSource) => ZIO.succeed {
        val result =
          resource.getLines().foldLeft(parser.parse("")) {
            case (parse, line ) =>
              parse.feed(line)
          }

        result.done match {
          case ParseResult.Fail(_, _, message) => throw new java.io.IOException(message)
          case ParseResult.Done(_, result) => result
          case ParseResult.Partial(_) => throw new java.io.IOException("Parsing of resource failed: Obtained a partial parse result")
        }

      }
    )

  def parseResourceMatrix(rows: Int, cols: Int, resource: String): Matrix[Double] = {
    val matrix =
      fromResourceEffect(resource, Atto.count(rows * cols, doubleParser)
        .map(data => Matrix(rows, cols, data.toVector)))

    Runtime.default.unsafeRunTask(matrix)
  }

  def parseResourceNonEmptyList(resource: String): NonEmptyList[Double] = {
    val list : Task[NonEmptyList[Double]] =
      fromResourceEffect(resource, many(doubleParser))
        .map(iter => NonEmptyList.fromIterableOption(iter).get)

    Runtime.default.unsafeRunTask(list)
  }

  lazy val sphere_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/sphere_func_data.txt")

  lazy val schwefel_102_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/schwefel_102_data.txt")

  lazy val high_cond_elliptic_rot_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/high_cond_elliptic_rot_data.txt")

  lazy val elliptic_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/elliptic_M_D2.txt")

  lazy val elliptic_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/elliptic_M_D10.txt")

  lazy val elliptic_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/elliptic_M_D30.txt")

  lazy val elliptic_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/elliptic_M_D50.txt")

  lazy val schwefel_206_data: Schwefel206Data = {
    val parser =
      for {
        shift <- Atto.count(100, doubleParser)
        a <- Atto.count(100 * 100, doubleParser)
      } yield Schwefel206Data(shift.toVector, Matrix(100, 100, a.toVector))

    val effect = fromResourceEffect("cec2005/schwefel_206_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val rosenbrock_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/rosenbrock_func_data.txt")

  lazy val griewank_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/griewank_func_data.txt")

  lazy val griewank_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/griewank_M_D2.txt")

  lazy val griewank_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/griewank_M_D10.txt")

  lazy val griewank_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/griewank_M_D30.txt")

  lazy val griewank_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/griewank_M_D50.txt")

  lazy val ackley_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/ackley_func_data.txt")

  lazy val ackley_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/ackley_M_D2.txt")

  lazy val ackley_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/ackley_M_D10.txt")

  lazy val ackley_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/ackley_M_D30.txt")

  lazy val ackley_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/ackley_M_D50.txt")

  lazy val rastrigin_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/rastrigin_func_data.txt")

  lazy val rastrigin_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/rastrigin_M_D2.txt")

  lazy val rastrigin_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/rastrigin_M_D10.txt")

  lazy val rastrigin_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/rastrigin_M_D30.txt")

  lazy val rastrigin_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/rastrigin_M_D50.txt")

  lazy val weierstrass_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/weierstrass_data.txt")

  lazy val weierstrass_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/weierstrass_M_D2.txt")

  lazy val weierstrass_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/weierstrass_M_D10.txt")

  lazy val weierstrass_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/weierstrass_M_D30.txt")

  lazy val weierstrass_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/weierstrass_M_D50.txt")

  lazy val schwefel_213_data: Schwefel213Data = {
    val parser =
      for {
        a <- Atto.count(100 * 100, doubleParser).map(_.toVector)
        b <- Atto.count(100 * 100, doubleParser).map(_.toVector)
        alpha <- Atto.count(100, doubleParser)
      } yield Schwefel213Data(
        Matrix(100, 100, a),
        Matrix(100, 100, b),
        alpha.toVector
      )

    val effect = fromResourceEffect("cec2005/schwefel_213_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val EF8F2_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/EF8F2_func_data.txt")

  lazy val scafferF6_func_data: NonEmptyList[Double] =
    parseResourceNonEmptyList("cec2005/E_ScafferF6_func_data.txt")

  lazy val scafferF6_M_D2: Matrix[Double] =
    parseResourceMatrix(2, 2, "cec2005/E_ScafferF6_M_D2.txt")

  lazy val scafferF6_M_D10: Matrix[Double] =
    parseResourceMatrix(10, 10, "cec2005/E_ScafferF6_M_D10.txt")

  lazy val scafferF6_M_D30: Matrix[Double] =
    parseResourceMatrix(30, 30, "cec2005/E_ScafferF6_M_D30.txt")

  lazy val scafferF6_M_D50: Matrix[Double] =
    parseResourceMatrix(50, 50, "cec2005/E_ScafferF6_M_D50.txt")

  lazy val hybrid_func1_data: HybridData = {
    val parser =
      many(Atto.count(100, doubleParser).map(x => NonEmptyVector.fromIterableOption(x).get))
        .map(list => HybridData(NonEmptyVector.fromIterableOption(list).get))

    val effect = fromResourceEffect("cec2005/hybrid_func1_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func1_M_D2: NonEmptyVector[Matrix[Double]] = {
    val dim = 2
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func1_M_D2.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func1_M_D10: NonEmptyVector[Matrix[Double]] = {
    val dim = 10
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func1_M_D10.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func1_M_D30: NonEmptyVector[Matrix[Double]] = {
    val dim = 30
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func1_M_D30.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func1_M_D50: NonEmptyVector[Matrix[Double]] = {
    val dim = 50
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func1_M_D50.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }


  lazy val hybrid_func2_data: HybridData = {
    val parser =
      many(Atto.count(100, doubleParser).map(x => NonEmptyVector.fromIterableOption(x).get))
        .map(list => HybridData(NonEmptyVector.fromIterableOption(list).get))

    val effect = fromResourceEffect("cec2005/hybrid_func2_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func2_M_D2: NonEmptyVector[Matrix[Double]] = {
    val dim = 2
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func2_M_D2.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func2_M_D10: NonEmptyVector[Matrix[Double]] = {
    val dim = 10
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func2_M_D10.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func2_M_D30: NonEmptyVector[Matrix[Double]] = {
    val dim = 30
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func2_M_D30.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func2_M_D50: NonEmptyVector[Matrix[Double]] = {
    val dim = 50
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func2_M_D50.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }


  lazy val hybrid_func3_data: HybridData = {
    val parser =
      many(Atto.count(100, doubleParser).map(x => NonEmptyVector.fromIterableOption(x).get))
        .map(list => HybridData(NonEmptyVector.fromIterableOption(list).get))

    val effect = fromResourceEffect("cec2005/hybrid_func3_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func3_M_D2: NonEmptyVector[Matrix[Double]] = {
    val dim = 2
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func3_M_D2.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func3_M_D10: NonEmptyVector[Matrix[Double]] = {
    val dim = 10
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func3_M_D10.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func3_M_D30: NonEmptyVector[Matrix[Double]] = {
    val dim = 30
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func3_M_D30.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func3_M_D50: NonEmptyVector[Matrix[Double]] = {
    val dim = 50
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func3_M_D50.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }



  lazy val hybrid_func4_data: HybridData = {
    val parser =
      many(Atto.count(100, doubleParser).map(x => NonEmptyVector.fromIterableOption(x).get))
        .map(list => HybridData(NonEmptyVector.fromIterableOption(list).get))

    val effect = fromResourceEffect("cec2005/hybrid_func4_data.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }


  lazy val hybrid_func4_M_D2: NonEmptyVector[Matrix[Double]] = {
    val dim = 2
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func4_M_D2.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func4_M_D10: NonEmptyVector[Matrix[Double]] = {
    val dim = 10
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func4_M_D10.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func4_M_D30: NonEmptyVector[Matrix[Double]] = {
    val dim = 30
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func4_M_D30.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }

  lazy val hybrid_func4_M_D50: NonEmptyVector[Matrix[Double]] = {
    val dim = 50
    val parser =
      Atto.count(10, Atto.count(dim * dim, doubleParser).map(x => Matrix(dim, dim, x.toVector)))
        .map(NonEmptyVector.fromIterableOption(_).get)

    val effect = fromResourceEffect("cec2005/hybrid_func4_M_D50.txt", parser)

    Runtime.default.unsafeRunTask(effect)
  }


}
