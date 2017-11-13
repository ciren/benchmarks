package benchmarks
package bbob2009

import shapeless._

import dimension._
import matrix._

trait F1Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F2Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F3Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F4Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F5Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F6Params [N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F7Params [N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F8Params [N<:Nat,A] { val params: (Dimension[N,A], A) }
trait F9Params [N<:Nat,A] { val params: (A, Matrix[N,N,A]) }
trait F10Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A]) }
trait F11Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A]) }
trait F12Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A]) }
trait F13Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F14Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A]) }
trait F15Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F16Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F17Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F18Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F19Params[N<:Nat,A] { val params: (A, Matrix[N,N,A]) }
trait F20Params[N<:Nat,A] { val params: (A, Dimension[N,A]) }
trait F21Params[N<:Nat,A] { val params: (A, Matrix[N,N,A]) }
trait F22Params[N<:Nat,A] { val params: (A, Matrix[N,N,A]) }
trait F23Params[N<:Nat,A] { val params: (Dimension[N,A], A, Matrix[N,N,A], Matrix[N,N,A]) }
trait F24Params[N<:Nat,A] { val params: (A, Dimension[N,A], Matrix[N,N,A], Matrix[N,N,A]) }
