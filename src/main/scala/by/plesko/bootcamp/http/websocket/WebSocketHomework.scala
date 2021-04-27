package by.plesko.bootcamp.http.websocket

import by.plesko.bootcamp.http.ServerAnswer.{Equal, Greater, Lower}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.concurrent.Queue
import fs2.{Pipe, Pull}
import org.http4s.HttpRoutes
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.util.Random

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

object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(gameRoutes.orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

  private val gameRoutes = {
    val gameLogicPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
      _.collect {
        case WebSocketFrame.Text(str, _) => str.trim.toInt
      }.pull.unconsN(2).flatMap {
        case Some((twoNumbers, tail)) =>
          val interval = twoNumbers.take(2).toList
          val randomNumber = Random.between(interval.head, interval.last)
          tail.map { guessNumber =>
            if (guessNumber < randomNumber) WebSocketFrame.Text(Lower.toString)
            else if (guessNumber > randomNumber) WebSocketFrame.Text(Greater.toString)
            else WebSocketFrame.Text(Equal.toString)
          }.pull.echo
        case None                     => Pull.done
      }.stream


    HttpRoutes.of[IO] {
      case GET -> Root / "start" =>
        for {
          queue <- Queue.bounded[IO, WebSocketFrame](2048)
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(gameLogicPipe)
          )
        } yield response
    }
  }
}

object GuessClient extends IOApp {

  private val uri = uri"ws://localhost:8080/start"

  private def guessNumber(min: Int, max: Int)(implicit client: WSConnectionHighLevel[IO]): IO[Boolean] = {
    val average = (min + max) / 2
    client.send(WSFrame.Text(average.toString)) *>
      client.receiveStream.collectFirst {
        case WSFrame.Text(data, _) if data == Lower.toString => guessNumber(average, max)
        case WSFrame.Text(data, _) if data == Greater.toString => guessNumber(min, average)
        case WSFrame.Text(data, _) if data == Equal.toString => IO.pure(true)
        case _ => IO.pure(false)
      }.compile.lastOrError.flatten
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val min = 0
    val max = 1000
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))
    clientResource.use { client =>
      for {
        _ <- client.sendMany(List(WSFrame.Text(min.toString), WSFrame.Text(max.toString)))
        game <- guessNumber(min, max)(client)
      } yield if (game) println("Thanks for game!") else println("Ops!")
    }.as(ExitCode.Success)
  }
}
