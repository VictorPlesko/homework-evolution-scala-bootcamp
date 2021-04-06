package by.plesko.bootcamp.effect

import by.plesko.bootcamp.effect.EffectsHomework1.IO
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

import scala.util.{Failure, Success, Try}

class EffectsHomework1Spec extends AnyFunSuite with Matchers {
  test("map is working correctly ") {
    forAll { (x: Int, y: Int) =>
      IO(x).map(_ => y).unsafeRunSync() shouldBe y
    }
  }

  test("flatMap is working correctly") {
    forAll { (x: Int, y: Int) =>
      IO(x).flatMap(_ => IO(y)).unsafeRunSync() shouldBe y
    }
  }

  test("*> is working correctly") {
    forAll { (x: Int, y: Int) =>
      (IO(x) *> IO(y)).unsafeRunSync() shouldBe y
    }
  }

  test("as is working correctly") {
    forAll { (x: Int, y: Int) =>
      (IO(x) as y).unsafeRunSync() shouldBe y
    }
  }

  test("void is working correctly") {
    forAll { (x: Int) =>
      IO(x).void.unsafeRunSync() shouldBe()
    }
  }

  test("attempt is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).attempt.unsafeRunSync() shouldBe Right(x)
    }
  }

  test("attempt is working correctly with exception") {
    forAll { (x: Throwable) =>
      IO.raiseError(x).attempt.unsafeRunSync() shouldBe Left(x)
    }
  }

  test("option is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).option.unsafeRunSync() shouldBe Some(x)
    }
  }

  test("option is working correctly with exception") {
    forAll { (x: Throwable) =>
      IO.raiseError(x).option.unsafeRunSync() shouldBe None
    }
  }

  test("handleErrorWith is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).handleErrorWith(_ => IO(0)).unsafeRunSync() shouldBe x
    }
  }

  test("handleErrorWith is working correctly with exception") {
    forAll { (x: Int) =>
      IO(x / 0).handleErrorWith(_ => IO(0)).unsafeRunSync() shouldBe 0
    }
  }

  test("redeem is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).redeem(_ => 0, value => value + 1).unsafeRunSync() shouldBe x + 1
    }
  }

  test("redeem is working correctly with exception") {
    forAll { (x: Int) =>
      IO(x / 0).redeem(_ => 0, value => value + 1).unsafeRunSync() shouldBe 0
    }
  }

  test("redeemWith is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).redeemWith(_ => IO(0), value => IO(value + 1)).unsafeRunSync() shouldBe x + 1
    }
  }

  test("redeemWith is working correctly with exception") {
    forAll { (x: Int) =>
      IO(x / 0).redeemWith(_ => IO(0), value => IO(value + 1)).unsafeRunSync() shouldBe 0
    }
  }

  test("unsafeRunSync is working correctly with valid value") {
    forAll { (x: Int) =>
      IO(x).unsafeRunSync() shouldBe x
    }
  }

  test("unsafeToFuture is working correctly with valid value") {
    import scala.concurrent.ExecutionContext.Implicits.global

    forAll { (x: Int) =>
      IO(x).unsafeToFuture().onComplete {
        case Failure(exception) => fail(s"${exception.getMessage}")
        case Success(value) => value shouldBe x
      }
    }
  }

  test("unsafeToFuture is working correctly with exception") {
    import scala.concurrent.ExecutionContext.Implicits.global

    forAll { (x: Throwable) =>
      IO.raiseError(x).unsafeToFuture().onComplete {
        case Failure(exception) => exception shouldBe x
        case Success(value) => fail(s"Not expected: $value")
      }
    }
  }

  test("pure is working correctly") {
    forAll { (x: Int) =>
      IO.pure(x).unsafeRunSync() shouldBe x
    }
  }

  test("fromEither is working correctly with valid value") {
    forAll { (x: Int) =>
      IO.fromEither(Right(x)).unsafeRunSync() shouldBe x
    }
  }

  test("fromEither is working correctly with exception") {
    forAll { (x: Throwable) =>
      a[Throwable] shouldBe thrownBy (IO.fromEither(Left(x)).unsafeRunSync())
    }
  }

  test("fromOption is working correctly with valid value") {
    forAll { (x: Int) =>
      IO.fromOption(Some(x))(throw new RuntimeException("test")).unsafeRunSync() shouldBe x
    }
  }

  test("fromOption is working correctly with exception") {
    a[RuntimeException] shouldBe thrownBy (IO.fromOption(None)(throw new RuntimeException("test")).unsafeRunSync())
  }

  test("fromTry is working correctly with valid value") {
    forAll { (x: Int) =>
      IO.fromTry(Try(x * 10)).unsafeRunSync() shouldBe x * 10
    }
  }

  test("fromTry is working correctly with exception") {
    forAll { (x: Int) =>
      a[ArithmeticException] shouldBe thrownBy (IO.fromTry(Try(x / 0)).unsafeRunSync())
    }
  }

  test("none is working correctly") {
    IO.none[Int].unsafeRunSync() shouldBe None
  }

  test("raiseError is working correctly") {
    forAll {(x: Throwable) =>
      a[Throwable] shouldBe thrownBy (IO.raiseError(x).unsafeRunSync())
    }
  }

  test("raiseUnless is working correctly when condition true") {
    forAll {(x: Throwable) =>
      IO.raiseUnless(cond = true)(x).unsafeRunSync() shouldBe ()
    }
  }

  test("raiseUnless is working correctly when condition false") {
    forAll {(x: Throwable) =>
      a[Throwable] shouldBe thrownBy ( IO.raiseUnless(cond = false)(x).unsafeRunSync())
    }
  }

  test("raiseWhen is working correctly when condition true") {
    forAll {(x: Throwable) =>
      a[Throwable] shouldBe thrownBy ( IO.raiseWhen(cond = true)(x).unsafeRunSync())
    }
  }

  test("raiseWhen is working correctly when condition false") {
    forAll {(x: Throwable) =>
      IO.raiseWhen(cond = false)(x).unsafeRunSync() shouldBe ()
    }
  }
}
