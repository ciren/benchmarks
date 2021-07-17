// package benchmarks
// package cec
// package cec2013
// package niching

// import benchmarks.Benchmarks._
// import benchmarks.cec.Helper
// import benchmarks.cec.cec2005.Benchmarks.f8f2
// import benchmarks.dimension._
// import benchmarks.implicits._
// import benchmarks.matrix._
// import shapeless._
// import shapeless.ops.nat._
// import spire.algebra._
// import spire.implicits._
// import spire.math._

//object Benchmarks {

  // Five Uneven Peak Trap
//  def f1[A: Field: Order] = fiveUnevenPeakTrap[A] _

  // Equal Maxima
  // def f2[A: Field: Trig] = equalMaxima[A] _

  // Uneven Decreasing Maxima
  // def f3[A: Field: NRoot: Trig] = unevenDecreasingMaxima[A] _

  // Himmelblau
  // def f4[A: Ring] = himmelblau[A] _

  // Six Hump Camel Back
  // def f5[A: Field] = sixHumpCamelback[A] _

  // Shubert
  // def f6[N <: Nat, A: Field: Trig] = shubert[N, A] _

  // Vincent
  // def f7[N <: Nat, A: Field: Trig] = vincent[N, A] _

  // Modified Rastrigin - All Global Optima
  // def f8[N <: Nat, A: Field: Trig](x: Dimension[N, A])(implicit P: F8Params[N]) =
  //   -(x zip P.params).mapSum {
  //     case (xi, ki) =>
  //       10.0 + 9.0 * cos(2.0 * pi * ki * xi)
  //   }

  // Composition Function 1
  // def f9[N <: Nat: GTEq1: ToInt, A: Field: NRoot: Ordering: Signed: Trig](
  //   x: Dimension[N, A]
  // )(implicit P: F9Params[N, A]): A =
  //   P.params match {
  //     case (o, fbias) => {
  //       val f: Dimension6[Dimension[N, A] => A] = Sized(
  //         griewank[N, A] _,
  //         griewank[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _
  //       )
  //       val b = Sized(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  //       val m = Sized.wrap[IndexedSeq[Matrix[N, N, A]], nat._6](Vector.fill(6)(Matrix.eye[N, A]))
  //       val λ = Sized(1.0, 1.0, 8.0, 8.0, 0.2, 0.2)
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  //       val h = Helper.hybrid[nat._6, N, A](b, o, m, f, λ, σ)
  //       h(x) + fbias
  //     }
  //   }

  // Composition Function 2
  // def f10[N <: Nat: GTEq1: ToInt, A: Field: NRoot: Ordering: Signed: Trig](
  //   x: Dimension[N, A]
  // )(implicit P: F10Params[N, A]): A =
  //   P.params match {
  //     case (o, fbias) => {
  //       val f: Dimension8[Dimension[N, A] => A] = Sized(
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _,
  //         spherical[N, A] _,
  //         spherical[N, A] _
  //       )
  //       val b = Sized(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  //       val m = Sized.wrap[IndexedSeq[Matrix[N, N, A]], nat._8](Vector.fill(8)(Matrix.eye[N, A]))
  //       val λ = Sized(1.0, 1.0, 10.0, 10.0, 0.1, 0.1, 1.0 / 7.0, 1.0 / 7.0)
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  //       val h = Helper.hybrid[nat._8, N, A](b, o, m, f, λ, σ)
  //       h(x) + fbias
  //     }
  //   }

  // Composition Function 3
  // def f11[N <: Nat: GTEq2: HasHead: ToInt, A: Field: NRoot: Ordering: Signed: Trig](
  //   x: Dimension[N, A]
  // )(implicit P: F11Params[N, A]): A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val f: Dimension6[Dimension[N, A] => A] = Sized(
  //         f8f2[N, A] _,
  //         f8f2[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  //       val λ = Sized(0.25, 0.1, 2.0, 1.0, 2.0, 5.0)
  //       val σ = Sized(1.0, 1.0, 2.0, 2.0, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._6, N, A](b, o, m, f, λ, σ)
  //       h(x) + fbias
  //     }
  //   }

  // Composition Function 4
  // def f12[N <: Nat: GTEq2: HasHead: ToInt, A: Field: NRoot: Ordering: Signed: Trig](
  //   x: Dimension[N, A]
  // )(implicit P: F12Params[N, A]): A =
  //   P.params match {
  //     case (o, m, fbias) => {
  //       val f: Dimension8[Dimension[N, A] => A] = Sized(
  //         rastrigin[N, A] _,
  //         rastrigin[N, A] _,
  //         f8f2[N, A] _,
  //         f8f2[N, A] _,
  //         weierstrass[N, A] _,
  //         weierstrass[N, A] _,
  //         griewank[N, A] _,
  //         griewank[N, A] _
  //       )
  //       val b = Sized(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  //       val λ = Sized(4.0, 1.0, 4.0, 1.0, 0.1, 0.2, 0.1, 1.0 / 40.0)
  //       val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0)
  //       val h = Helper.hybrid[nat._8, N, A](b, o, m, f, λ, σ)
  //       h(x) + fbias
  //     }
  //   }
//}
