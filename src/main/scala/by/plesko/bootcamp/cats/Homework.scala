package by.plesko.bootcamp.cats

import cats.Functor

class Homework {
  trait Applicative[F[_]] extends Functor[F] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fa, fab)((a, f) => f(a))                                   //flatmap(fab)(f => map(fa)(f))

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
      join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] =
      flatMap(mma)(identity)

    def map[A,B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      join(map(ma)(a => map(mb)(b => f(a,b))))
  }
}
