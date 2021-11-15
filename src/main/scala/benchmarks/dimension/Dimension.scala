// package benchmarks
// package dimension

// import scalaz.Scalaz._
// import shapeless._
// import shapeless.ops.nat.ToInt

// import cilib._

// object Dimension {

//   def fill[N <: Nat: ToInt, A](f: => A): Dimension[N, A] = {
//     val size = implicitly[ToInt[N]].apply
//     Sized.wrap(Vector.fill(size)(f))
//   }

//   def random[N <: Nat: ToInt, A: Field](interval: Interval[Double]): RVar[Dimension[N, A]] = {
//     val size = implicitly[ToInt[N]].apply
//     val A    = implicitly[Field[A]]
//     val xs   = Vector.fill(size)(Dist.uniform(interval) map (A.fromDouble)).sequence
//     xs map Sized.wrap[IndexedSeq[A], N]
//   }

// }
