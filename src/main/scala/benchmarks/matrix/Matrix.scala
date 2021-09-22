package benchmarks
package matrix

object Matrix {

  def wrap[A](cols: IndexedSeq[A]*): Matrix[A] =
    cols.map(_.toVector).toVector

  // identity matrix
  def identity[A: Ring](size: Int): Matrix[A] = {
    val A = implicitly[Ring[A]]
    val elements =
      for {
        c <- 0 until size
      } yield for {
        r <- 0 until size
        v = if (c == r) A.one else A.zero
      } yield v
    Matrix.wrap(elements: _*)
  }

  // alpha matrix
  def alpha[A: Field: NRoot](alpha: Double, size: Int): Matrix[A] = {
    //val size = implicitly[ToInt[N]].apply
    val A    = implicitly[Field[A]]
    val elements =
      for {
        c <- 0 until size
      } yield for {
        r <- 0 until size
        v = if (c == r) A.fromDouble(alpha) ** (0.5 * (c / (size - 1.0)))
        else A fromDouble 0.0
      } yield v
    Matrix.wrap(elements: _*)
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
