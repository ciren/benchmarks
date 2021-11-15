package benchmarks
package matrix

import cilib.NonEmptyVector

final case class Matrix[A] private (val nRows: Int, val nCols: Int, data: Vector[A]) {

  // matrix transpose
  def transpose =
    Matrix(nCols, nRows, toVector.transpose.flatten.toVector)

  def rotate(x: NonEmptyVector[A])(implicit N: Numeric[A]): NonEmptyVector[A] = {
    val xVec = x.toChunk.toVector
    def innerProduct(other: Vector[A]): A =
      xVec.zip(other)
        .map { case (xi, oi) => N.times(xi, oi) }
        .foldLeft(N.zero)(N.plus(_, _))

    val t = transpose.toVector.map(z => innerProduct(z))

    NonEmptyVector.fromIterableOption(t).get
  }


  def toVector: Vector[Vector[A]] = {
    def go(input: Vector[A], acc: Vector[Vector[A]]): Vector[Vector[A]] = {
      if (input.length > nCols) go(input.drop(nCols), acc :+ input.take(nCols))
      else acc :+ input
    }

    go(data, Vector.empty)
  }

  def submatrix(startRow: Int, endRow: Int, startCol: Int, endCol: Int) = {
    Matrix(
      endRow - startRow,
      endCol - startCol,
      data.slice(startRow * nCols + startCol, endRow * nCols + endCol))
  }

  def getRow(row: Int): Option[Vector[A]] = {
    val slice = data.slice(row * nCols, nCols)
    if (slice.isEmpty) None else Some(slice)
  }

  def dropRow(n: Int): Matrix[A] =
    if (n >= nRows) Matrix.empty[A]
    else Matrix(nRows-n, nCols, data.drop(n * nCols))

  def takeRow(n: Int): Matrix[A] =
    if (n >= nRows) this
    else Matrix(n, nCols, data.take(n * nCols))

  def mapRow(f: Vector[A] => Vector[A]): Matrix[A] = {
    val modified = toVector.map(f)
    Matrix(nRows, modified.head.length, modified.flatten)
  }


}


object Matrix {

  def empty[A] = Matrix(0,0,Vector.empty[A])

  def matrix[A](rows: Int, cols: Int)(f: (Int, Int) => A): Matrix[A] = {
    val builder = Vector.newBuilder[A]

    for {
      i <- 0 until rows
      j <- 0 until cols
    } builder += f(i,j)

    Matrix(rows, cols, builder.result())
  }

  // identity matrix
  def identity[A:Numeric](size: Int): Matrix[A] = {
    val A = implicitly[Numeric[A]]
    matrix(size, size) { (i: Int, j: Int) => if (i == j) A.one else A.zero }
  }

  // alpha matrix
  def alpha(alpha: Double, size: Int): Matrix[Double] =
    matrix(size, size) { (i: Int, j: Int) =>
      if (i == j) math.pow(alpha, 0.5 * (j / (size - 1.0))) else 0.0
    }

//   def random[N <: Nat: GTEq1: ToInt](i: Interval[Double]): RVar[Matrix[N, N, Double]] = {
//     val size = implicitly[ToInt[N]].apply
//     val elements =
//       for {
//         c <- 0 until size
//       } yield for {
//         r <- 0 until size
//       } yield Dist uniform i
//     val transformed = elements.toVector.traverse(_.toVector.sequence)
//     transformed.map(els => Matrix.wrap[N, N, Double](els: _*))
//   }


  // def rotation[N <: Nat: GTEq1: ToInt]: RVar[Matrix[N, N, Double]] = {
  //   def helper(
  //     remaining: Vector[Dimension[N, Double]],
  //     us: Vector[Dimension[N, Double]]
  //   ): Vector[Dimension[N, Double]] = remaining match {
  //     case v +: vs =>
  //       val u = us.headOption.map(_ => v - us.map(_ project v).reduce(_ + _)).getOrElse(v)
  //       helper(vs, us :+ u)
  //     case _ => us
  //   }

  //   for {
  //     m <- Matrix.random[N](Interval(-1.0, 1.0))
  //   } yield Sized.wrap(helper(m.toVector, Vector())) map (_.normalized)
  // }
}
