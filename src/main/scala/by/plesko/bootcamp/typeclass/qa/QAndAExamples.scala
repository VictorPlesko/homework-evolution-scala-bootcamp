package by.plesko.bootcamp.typeclass.qa

object QAndAExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
  }


  implicit class SemigroupOps[A: Semigroup](x: A) {
    def combine(y: A): A = Semigroup[A].combine(x, y)
  }
  // 1.2. Implement Semigroup for Long, String

  //  implicit val longSemigroup: Semigroup[Long] = new Semigroup[Long] {
  //    override def combine(x: Long, y: Long): Long = x + y
  //  }
  //
  //  implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
  //    override def combine(x: String, y: String): String = x + y
  //  }

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists

  // combineAll(List(1, 2, 3)) == 6

  def combineAll[A: Semigroup](list: List[A]): A = list.reduce((x, y) => x.combine(y))

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  def combineAll[A: Semigroup](list: List[A], startingElement: A): A = list.fold(startingElement)(_ combine _)

  // combineAll(List(1, 2, 3), 0) == 6
  // combineAll(List(), 1) == 1

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }
  // 2.2. Implement Monoid for Long, String

  implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 0L

    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists

  // combineAll(List(1, 2, 3)) == 6

  def combineAllMonoid[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(_ combine _)

  // 2.4. Implement Monoid for Option[A]

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xVal), Some(yVal)) => Some(xVal.combine(yVal))
      case (x, y)                   => x.orElse(y)
    }
  }

  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)

  implicit def function1Monoid[A, B: Monoid]: Monoid[Function1[A, B]] = new Monoid[Function1[A, B]] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = (a: A) => x(a).combine(y(a))
  }
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)

  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_] : Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]
  }

  implicit class SemigroupalOps[F[_] : Semigroupal, A](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }
  // 4.2. Implement Semigroupal for Option

  //  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
  //    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
  //      case (Some(aVal), Some(bVal)) => Some((aVal, bVal))
  //      case (_, _)                   => None
  //    }
  //  }
  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  implicit class tuple2SemigroupalOps[F[_] : Semigroupal : Functor, A, B](fa: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = fa match {
      case (firstVal, secondVal) => firstVal.product(secondVal).fmap(f.tupled)
    }
  }
  // (Option(1), Option(2)).mapN(_ + _) == Some(3)
  // (Option(1), None).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map

  implicit def mapFunctor[T]: Functor[Map[T, *]] = new Functor[Map[T, *]] {
    override def fmap[A, B](fa: Map[T, A])(f: A => B): Map[T, B] = fa.view.mapValues(f).toMap
  }

  implicit def mapSemigroupal[T]: Semigroupal[Map[T, *]] = new Semigroupal[Map[T, *]] {
    override def product[A, B](fa: Map[T, A], fb: Map[T, B]): Map[T, (A, B)] = {
      for {
        x <- fa
        y <- fb.get(x._1)
      } yield (x._1 -> (x._2, y))
    }
  }

  println((Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc"))

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val applicativeEither: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)

    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = (fa, fb) match {
      case (Right(xVal), Right(yVal)) => Right((xVal, yVal))
      case (Left(value), _)           => Left(value)
      case (_, Left(value))           => Left(value)
    }

    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa.map(f)
  }

  implicit val applicativeOption: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(aVal), Some(bVal)) => Some((aVal, bVal))
      case (_, _)                   => None
    }
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldLeft(Option(List.empty[B]))((acc, el) => f(el) match {
      case Some(value) => acc.map(_ :+ value)
      case _ => None
    })
  }

  def traverseAll[F[_]: Applicative, A, B](as:List[A])(f: A => F[B]):F[List[B]] = {
    as.foldLeft(implicitly[Applicative[F]].pure(List.empty[B])){(acc, el) =>
      (acc, f(el)).mapN(_ :+ _)
    }
  }

  println(traverseAll(List(1, 2, 3)) { i =>
    Option.when(i % 2 == 1)(i)
  } == None)

  println(traverseAll(List(1L, 2L, 3L)) { i =>
    Some(i + 1L): Option[Long]
  } == Some(List(2L, 3L, 4L)))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]]{
    def foldLeft[A,B](fa: F[A], z:B)(f: (B, A) => B): B
  }

  object Foldable{
    def apply[F[_]: Foldable]: Foldable[F] = implicitly[Foldable[F]]
  }

  implicit class FoldableOps[F[_] : Foldable, A](fa: F[A]){
    def foldLeft1[B](z:B)(f: (B, A) => B): B = Foldable[F].foldLeft(fa,z)(f)
  }
  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library

  implicit val foldableOps: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa match {
      case x :: xs => foldLeft(xs,f(z,x))(f)
      case Nil => z
    }
  }

  println(List(1,2,3).foldLeft1(0)(_ + _))
  // 6.3. Implement `traverse` for all Foldables instead of List

  // I don't know how to implement this method :(

  /*def traverseFoldables[F[_]: Applicative, R[_]: Foldable, A, B](as:R[A])(f: A => F[B]):F[R[B]] = {
    as.foldLeft1(implicitly[Applicative[F]].pure(Monoid[R[B]].empty)){(acc, el) =>
      (acc, f(el)).mapN()
    }
  }*/

}

