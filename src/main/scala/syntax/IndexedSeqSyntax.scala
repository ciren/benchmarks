package benchmarks
package syntax

import scala.language.implicitConversions

import scalaz._
import Scalaz._

import spire.algebra.{Field,Ring}
import spire.implicits._

final class IndexedSeqOps[A](x: IndexedSeq[A]) {
  def mapSum[B](f: A => B)(implicit ev: Ring[B]): B =
    x.foldLeft(ev.zero)((b: B, a: A) => b + f(a))

  def mapProduct[B](f: A => B)(implicit ev: Field[B]): B =
    x.foldLeft(ev.one)((b: B, a: A) => b * f(a))

  def pairs =
    x.sliding(2).toList.map { case Seq(x1, x2) => (x1, x2) }

  def traverse[G[_]:Applicative,B](f: A => G[B]) =
    x.toList.traverse(f)
}

trait IndexedSeqSyntax {
  implicit def ToIndexedSeqOps[A](v: IndexedSeq[A]): IndexedSeqOps[A] = new IndexedSeqOps[A](v)
}
