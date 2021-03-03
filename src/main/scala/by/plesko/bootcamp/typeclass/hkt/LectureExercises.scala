package by.plesko.bootcamp.typeclass.hkt

object LectureExercises extends App {
  // Exercise 12. Implement Functor for `Disjunction`
  trait BiFunctor[F[_, _]] {
    def fmapLeft[A, B, C](fa: F[A, B])(f: A => C): F[C, B]
    def fmapRight[A, B, C](fa: F[A, B])(f: B => C): F[A, C]
  }

  object BiFunctor {
    def apply[F[_,_]: BiFunctor]: BiFunctor[F] = implicitly[BiFunctor[F]]
  }

  implicit class BiFunctorOps[F[_, _] : BiFunctor, A, B](fa: F[A, B]) {
    def fmapLeft[C](f: A => C): F[C, B] = BiFunctor[F].fmapLeft(fa)(f)

    def fmapRight[C](f: B => C): F[A, C] = BiFunctor[F].fmapRight(fa)(f)
  }

  implicit val tupleMap: BiFunctor[Tuple2] = new BiFunctor[Tuple2] {
    override def fmapLeft[A, B, C](fa: (A, B))(f: A => C): (C,B) = fa match {
      case (left, right) => (f(left), right)
    }

    override def fmapRight[A, B, C](fa: (A, B))(f: B => C):(A,C) = fa match {
      case (left, right) => (left, f(right))
    }
  }

  println((2,3).fmapLeft(_+3))
  println((2,3).fmapRight(_+2))

}
