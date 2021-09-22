// package benchmarks
// package bbob2009

// import dimension._
// import dimension.implicits._

// object Helpers {

//   def sign[A: Order](x: A)(implicit A: Field[A]): A =
//     if (x < 0) A.fromDouble(-1)
//     else if (x == 0) x
//     else A.fromDouble(1)

//   def oscillate[A: Field: Order: Signed: Trig](x: A): A = {
//     val c1    = if (x > 0) 10.0 else 5.5
//     val c2    = if (x > 0) 7.9 else 3.1
//     val x_hat = if (x != 0) log(abs(x)) else x
//     sign(x) * exp(x_hat + 0.049 * (sin(c1 * x_hat) + sin(c2 * x_hat)))
//   }

//   def asymmetric[N <: Nat, A: Field: Order: NRoot](β: Double): Dimension[N, A] => Dimension[N, A] =
//     x => {
//       val D = x.size
//       x.zipWithIndex map {
//         case (xi, i) =>
//           if (xi <= 0) xi
//           else {
//             val exponent =
//               implicitly[Field[A]].fromDouble(β) * (i / (D - 1)) * sqrt(xi) + 1
//             xi fpow exponent
//           }
//       }
//     }
// }
