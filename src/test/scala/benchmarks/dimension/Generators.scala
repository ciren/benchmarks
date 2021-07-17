// package benchmarks
// package dimension

// import scalaz._
// import Scalaz._
// import scalaz.scalacheck.ScalaCheckBinding._

// import shapeless._

// import org.scalacheck._
// import org.scalacheck.Gen.Choose

// object Generators {
//   def c[A:Choose](l: A, u: A) = Gen.choose(l, u)

//   def gen1[A:Choose](l: A, u: A): Gen[Dimension1[A]] =
//     c(l, u).map(Sized(_))

//   def gen2[A:Choose](l: A, u: A): Gen[Dimension2[A]] =
//     (c(l, u) |@| c(l, u)) { (a, b) => Sized(a, b) }

//   def gen2D[A:Choose](d1: (A, A), d2: (A, A)) =
//     (c(d1._1, d1._2) |@| c(d2._1, d2._2)) { (a, b) => Sized(a, b) }

//   def gen3[A:Choose](l: A, u: A) =
//     (c(l, u) |@| c(l, u) |@| c(l, u)) { Sized(_, _, _) }

//   def gen3D[A:Choose](d1: (A, A), d2: (A, A), d3: (A, A)) =
//     (c(d1._1, d1._2) |@| c(d2._1, d2._2) |@| c(d3._1, d3._2)) { Sized(_, _, _) }

//   def gen4[A:Choose](l: A, u: A): Gen[Dimension4[A]] =
//     (c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l,u)) { Sized(_, _, _, _) }

//   def gen5[A:Choose](l: A, u: A) =
//     (c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l, u)) { Sized(_, _, _, _, _) }

//   def gen6[A:Choose](l: A, u: A) =
//     (c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l, u) |@| c(l, u)) {
//       Sized(_, _, _, _, _, _)
//     }

//   def gen10[A:Choose](l: A, u: A): Gen[Dimension[_10,A]] =
//     for {
//       xs <- Gen.containerOfN[Vector,A](10, c(l, u))
//     } yield Sized.wrap[IndexedSeq[A],_10](xs)

//   def gen16[A:Choose](l: A, u: A): Gen[Dimension[_16,A]] =
//     for {
//       xs <- Gen.containerOfN[Vector,A](16, c(l, u))
//     } yield Sized.wrap[IndexedSeq[A],_16](xs)

//   def gen30[A:Choose](l: A, u: A): Gen[Dimension[_30,A]] =
//     for {
//       xs <- Gen.containerOfN[Vector,A](30, c(l, u))
//     } yield Sized.wrap[IndexedSeq[A],_30](xs)

//   def gen50[A:Choose](l: A, u: A): Gen[Dimension[_50,A]] =
//     for {
//       xs <- Gen.containerOfN[Vector,A](50, c(l, u))
//     } yield Sized.wrap[IndexedSeq[A],_50](xs)

//   def genSized[A:Choose](l: A, u: A): Gen[Dimension[_10,A]] =
//     for {
//       xs <- Gen.containerOfN[Vector,A](10, c(l, u))
//     } yield Sized.wrap(xs)

//   def genConst[A:Choose](v: A) = genSized(v, v)
// }
