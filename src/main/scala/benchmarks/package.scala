import cilib.Input

package object benchmarks {

  // implicit def input[N <: Nat: ToInt, A]: Input[({ type T[A] = Dimension[N, A] })#T] =
  //   new Input[({ type T[A] = Dimension[N, A] })#T] {
  //     def toInput[B](a: NonEmptyList[B]): Dimension[N, B] = {
  //       val dim = implicitly[ToInt[N]].apply
  //       if (dim != a.size) sys.error("Input vector dimension is not the same as the benchmark function")
  //       else Sized.wrap[IndexedSeq[B], N](a.toVector)
  //     }
  //   }

  import zio.prelude._
  import zio.test.Assertion
  import zio.test.AssertionM.Render.param

  def isLongerThan(reference: Int): AssertionF[NonEmptyList] =
    new AssertionF[NonEmptyList] {
      def apply[A]: Assertion[NonEmptyList[A]] =
        Assertion.assertion("isLongerThan")(param(reference))(_.length >= reference)
    }



  // TODO: This type needs a better name
  object AtLeast2List extends NewtypeSmartF[NonEmptyList](isLongerThan(2))
  type AtLeast2List[x] = AtLeast2List.Type[x]

  implicit val AtLeast2ListToInput: Input[AtLeast2List] =
    new Input[AtLeast2List] {
      def toInput[A](a: scalaz.NonEmptyList[A]): AtLeast2List[A] =
        AtLeast2List.make(NonEmptyList.fromIterable(a.head, a.tail.toList)) match {
          case ZValidation.Failure(w, e) => sys.error("asd")
          case ZValidation.Success(w, a) => a
        }
    }



  implicit val Tuple3ToInput: Input[({ type lambda[A] = (A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A) =
        a.list.toList match {
          case a :: b :: c :: Nil => (a, b, c)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 3 but got length ${a.size}")
        }
    }

  implicit val Tuple4ToInput: Input[({ type lambda[A] = (A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A) =
        a.list.toList match {
          case a :: b :: c :: d :: Nil => (a, b, c, d)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 4 but got length ${a.size}")
        }
    }

  implicit val Tuple5ToInput: Input[({ type lambda[A] = (A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A) =
        a.list.toList match {
          case a :: b :: c :: d :: e :: Nil => (a, b, c, d, e)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 5 but got length ${a.size}")
        }
    }

  implicit val Tuple6ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A, A) =
        a.list.toList match {
          case a :: b :: c :: d :: e :: f :: Nil => (a, b, c, d, e, f)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 6 but got length ${a.size}")
        }
    }

  implicit val Tuple7ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A, A, A) =
        a.list.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: Nil => (x1, x2, x3, x4, x5, x6, x7)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 7 but got length ${a.size}")
        }
    }

  implicit val Tuple8ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A, A, A, A) =
        a.list.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 8 but got length ${a.size}")
        }
    }

  implicit val Tuple9ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A, A, A, A, A) =
        a.list.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 9 but got length ${a.size}")
        }
    }

  implicit val Tuple10ToInput: Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A, A) })#lambda] =
    new Input[({ type lambda[A] = (A, A, A, A, A, A, A, A, A, A) })#lambda] {
      def toInput[A](a: scalaz.NonEmptyList[A]): (A, A, A, A, A, A, A, A, A, A) =
        a.list.toList match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
          case _ => sys.error(s"Input vector dimension is not the same as the benchmark function. Expected length 10 but got length ${a.size}")
        }
    }

}
