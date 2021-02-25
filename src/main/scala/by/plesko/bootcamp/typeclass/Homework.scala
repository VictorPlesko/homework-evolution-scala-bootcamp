package by.plesko.bootcamp.typeclass

object TypeclassTask extends App {
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit j: HashCode[A]): Int = {
      j.hash(x)
    }
  }

  implicit val hashCodeForString: HashCode[String] = str => str.foldLeft(1)((acc, symbol) => 31 * acc + symbol)

  println("abc".hash)
}

object Task1 {
  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  implicit val showUser: Show[User] = user => s"User(id: ${user.id}, name: ${user.name})"

  // TODO: create syntax for Show so i can do User("1", "Oleg").show
  implicit class ShowOps[A](x: A) {
    def show(implicit j: Show[A]): String = j.show(x)
  }

  println(User("1", "Oleg").show)
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  // TODO: create Parse instance for User

  implicit val parserForUser: Parse[User] = str => str.split("<separator>").toList match {
    case id :: name :: Nil => Right(User(id, name))
    case _ => Left("Invalid string!")
  }

  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)

  implicit class ParserObs(x: String){
    def parse[T](implicit j: Parse[T]): Either[Error, T] = j.parse(x)
  }

  println("Lslsls".parse[User])
}

object Task4 extends App {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  trait TypeSaleEquals[T]{
    def eq(x:T, y:T): Boolean
  }

  implicit class TypeSaleEqualsOps[A](x: A){
    def ===(y: A)(implicit j: TypeSaleEquals[A]): Boolean = j.eq(x,y)
  }

  implicit val equalsForInt: TypeSaleEquals[Int] = (x,y) => x == y

  println(3 === 4)
  //println(3 === "str")
}

