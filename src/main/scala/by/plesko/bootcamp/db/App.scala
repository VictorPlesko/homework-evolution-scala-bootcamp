package by.plesko.bootcamp.db

import by.plesko.bootcamp.db.database_utils.{DbCommon, DbService, DbTransactor}
import by.plesko.bootcamp.db.http.HttpService
import by.plesko.bootcamp.db.http.HttpService.deleteDataRoutes
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import doobie.implicits._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext

object App extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    DbTransactor.transactor[IO].use { tr =>
      for {
        _ <- DbCommon.setup().transact(tr)
        dbService = new DbService[IO](tr)
        routes = (HttpService.readDataRoutes(dbService)
          <+> HttpService.insertDataRoutes(dbService)
          <+> HttpService.updateDataRoutes(dbService)
          <+> deleteDataRoutes(dbService))
          .orNotFound
        _ <- BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(8080, "localhost")
          .withHttpApp(routes)
          .serve
          .compile
          .drain
      } yield ExitCode.Success
    }
  }
}
