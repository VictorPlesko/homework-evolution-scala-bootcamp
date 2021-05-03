package by.plesko.bootcamp.db

import by.plesko.bootcamp.db.AdditionalVar.UUIDQueryVar
import cats.data.Validated
import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.dsl.impl.Root
import io.circe.generic.auto._
import org.http4s.implicits._
import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec._

import java.time.{LocalDate, Year}
import java.util.UUID

object AdditionalVar {
  implicit val UUIDDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed to decode UUID", t.getMessage))
      .toValidatedNel
  }
  object UUIDQueryVar extends QueryParamDecoderMatcher[UUID]("id")
}

object HttpService {
  def readDataRoutes(dbService: DbService[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case GET -> Root / "getAllBooks" =>
        for {
          books <- dbService.readAllBooks
          response <- Ok(books)
        } yield response

      case GET -> Root / "getAllAuthors" =>
        for {
          authors <- dbService.readAllAuthors
          response <- Ok(authors)
        } yield response

      case GET -> Root / "getBook" :? UUIDQueryVar(id) =>
        for {
          bookOption <- dbService.readBook(id)
          response <- bookOption.fold(NotFound())(Ok(_))
        } yield response

      case GET -> Root / "getAuthor" :? UUIDQueryVar(id) =>
        for {
          authorOption <- dbService.readAuthor(id)
          response <- authorOption.fold(NotFound())(Ok(_))
        } yield response
    }
  }

  def insertDataRoutes(dbService: DbService[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req@POST -> Root / "insertBook" =>
        req.as[Book].flatMap { book =>
          dbService.insertBook(book).flatMap(_ => NoContent())
        }

      case req@POST -> Root / "insertAuthor" =>
        req.as[Author].flatMap { author =>
          dbService.insertAuthor(author).flatMap(_ => NoContent())
        }
    }
  }

  def updateDataRoutes(dbService: DbService[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req@PUT -> Root / "updateBook" :? UUIDQueryVar(id) =>
        req.as[BookWithoutID].flatMap { book =>
          dbService.updateBook(id, book).flatMap(_.fold(NotFound())(_ => NoContent()))
        }
      case req@PUT -> Root / "updateAuthor" :? UUIDQueryVar(id) =>
        req.as[AuthorWithoutID].flatMap { author =>
          dbService.updateAuthor(id, author).flatMap(_.fold(NotFound())(_ => NoContent()))
        }
    }
  }

  def deleteDataRoutes(dbService: DbService[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case DELETE -> Root / "deleteBook" :? UUIDQueryVar(id) =>
        dbService.deleteBook(id).flatMap(_.fold(NotFound())(_ => NoContent()))

      case DELETE -> Root / "deleteAuthor" :? UUIDQueryVar(id) =>
        dbService.deleteAuthor(id).flatMap(_.fold(NotFound())(_ => NoContent()))
    }
  }
}
