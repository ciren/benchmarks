import zio.prelude._
import cilib.Input
import cilib.NonEmptyVector

package object benchmarks {

  // TODO: This type needs a better name
  object AtLeast2List extends Newtype[NonEmptyVector[Double]]
  type AtLeast2List = AtLeast2List.Type



  // implicit val AtLeast2ListToInput: Input[AtLeast2List.Type] =
  //   new Input[AtLeast2List] {
  //     def toInput[A](a: NonEmptyVector[A]): AtLeast2List[A] =
  //       AtLeast2List.make(a) match {
  //         case ZValidation.Failure(w, e) => sys.error("asd")
  //         case ZValidation.Success(w, result) => result
  //       }
  //   }

  implicit val Tuple3ToInput: Input[({ type lambda[A] = (A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A) =
        a.toChunk.toList match {
          case a :: b :: c :: Nil => (a, b, c)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 3 but got length ${a.size}")
        }
    }

  implicit val Tuple4ToInput: Input[({ type lambda[A] = (A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A) =
        a.toChunk.toList match {
          case a :: b :: c :: d :: Nil => (a, b, c, d)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 4 but got length ${a.size}")
        }
    }

  implicit val Tuple5ToInput: Input[({ type lambda[A] = (A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A) =
        a.toChunk.toList match {
          case a :: b :: c :: d :: e :: Nil => (a, b, c, d, e)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 5 but got length ${a.size}")
        }
    }

  implicit val Tuple6ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A, A) =
        a.toChunk.toList match {
          case a :: b :: c :: d :: e :: f :: Nil => (a, b, c, d, e, f)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 6 but got length ${a.size}")
        }
    }

  implicit val Tuple7ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A, A, A) =
        a.toChunk.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: Nil => (x1, x2, x3, x4, x5, x6, x7)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 7 but got length ${a.size}")
        }
    }

  implicit val Tuple8ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A, A, A, A) =
        a.toChunk.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 8 but got length ${a.size}")
        }
    }

  implicit val Tuple9ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A, A, A, A, A) =
        a.toChunk.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 9 but got length ${a.size}")
        }
    }

  implicit val Tuple10ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: NonEmptyVector[A]): (A, A, A, A, A, A, A, A, A, A) =
        a.toChunk.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 10 but got length ${a.size}")
        }
    }

}
