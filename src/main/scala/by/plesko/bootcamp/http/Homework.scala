package by.plesko.bootcamp.http

import by.plesko.bootcamp.http.Error.NotFoundID
import by.plesko.bootcamp.http.ServerAnswer.{Equal, Greater, Lower}
import cats.Functor
import cats.data.Validated
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

trait Repo[F[_]] {
  def add(id: UUID, number: Int): F[Unit]
  def get(id: UUID): F[Option[Int]]
  def remove(id: UUID): F[Unit]
}

final case class RefRepo[F[_] : Functor](
                                          private val ref: Ref[F, Map[UUID, Int]]
                                        ) extends Repo[F] {

  override def add(id: UUID, number: Int): F[Unit] = ref.update(_ + (id -> number))

  override def get(id: UUID): F[Option[Int]] = ref.get.map(_.get(id))

  override def remove(id: UUID): F[Unit] = ref.update(_ - id)
}

object RefRepo {
  def of[F[_] : Functor : Sync]: F[RefRepo[F]] =
    Ref.of[F, Map[UUID, Int]](Map.empty[UUID, Int]).map(RefRepo(_))
}

sealed trait Answer
object ServerAnswer {
  final case object Lower extends Answer {
    override def toString: String = "Your number is less than the given."
  }
  final case object Greater extends Answer {
    override def toString: String = "Your number is greater than the given."
  }
  final case object Equal extends Answer {
    override def toString: String = "Congratulations! Your number is equal to the given one."
  }
}

sealed trait Error
object Error {
  final case object NotFoundID extends Answer with Error {
    override def toString: String = "The specified ID was not found!"
  }
}

object AdditionalVar {
  object MinVar extends QueryParamDecoderMatcher[Int]("min")
  object MaxVar extends QueryParamDecoderMatcher[Int]("max")
  object GuessVar extends QueryParamDecoderMatcher[Int]("guess")
  implicit val UUIDDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed to decode UUID", t.getMessage))
      .toValidatedNel
  }
  object UUIDVar extends QueryParamDecoderMatcher[UUID]("id")
}

object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    RefRepo.of[IO].flatMap(ref =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 8080, host = "localhost")
        .withHttpApp(gameRoutes(ref).orNotFound)
        .serve
        .compile
        .drain
        .as(ExitCode.Success))
  }

  private def gameRoutes(refRepo: RefRepo[IO]) = {
    import by.plesko.bootcamp.http.AdditionalVar._

    HttpRoutes.of[IO] {
      case GET -> Root / "start" :? MinVar(min) +& MaxVar(max) =>
        for {
          id <- IO.pure(UUID.randomUUID())
          randomNumber <- IO(Random.between(min, max))
          _ <- refRepo.add(id, randomNumber)
          response <- Ok(id.toString)
        } yield response

      case GET -> Root / "guess" :? UUIDVar(id) +& GuessVar(clientNumber) =>
        val response = for {
          correctNumberOption <- refRepo.get(id)
        } yield {
          correctNumberOption.map { correctNumber =>
            if (clientNumber < correctNumber) Ok(Lower.toString)
            else if (clientNumber > correctNumber) Ok(Greater.toString)
            else refRepo.remove(id) *> Ok(Equal.toString)
          }
        }
        response.flatMap(_.fold(NotFound(NotFoundID.toString))(identity))
    }
  }
}
object GuessClient extends IOApp {

  private val uri = uri"http://localhost:8080"

  private def guessNumber(clientId: String, min: Int, max: Int)(implicit client: Client[IO]): IO[Boolean] = {
    val average = (min + max) / 2
    client.expect[String]((uri / "guess").withQueryParam(key = "id", value = clientId).withQueryParam(key = "guess", value = average))
      .flatMap { str =>
        if (str == Lower.toString) guessNumber(clientId, average, max)
        else if (str == Greater.toString) guessNumber(clientId, min, average)
        else if (str == Equal.toString) IO.pure(true)
        else IO.pure(false)
      }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val min = 0
    val max = 1000
    BlazeClientBuilder[IO](ExecutionContext.global)
      .resource
      .use { client =>
        for {
          clientId <- client.expect[String]((uri / "start").withQueryParam(key = "min", value = min).withQueryParam(key = "max", value = max))
          _ <- guessNumber(clientId, min, max)(client)
        } yield ()
      }.as(ExitCode.Success)
  }
}
