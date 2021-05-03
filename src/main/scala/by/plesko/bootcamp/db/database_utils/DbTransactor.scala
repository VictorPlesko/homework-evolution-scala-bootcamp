package by.plesko.bootcamp.db.database_utils

import by.plesko.bootcamp.db.config.DbConfig
import cats.effect.{Async, Blocker, ContextShift, Resource}
import doobie.{ExecutionContexts, Transactor}
import doobie.hikari.HikariTransactor

object DbTransactor {
  def transactor[F[_] : ContextShift : Async]: Resource[F, Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](16)
      be <- Blocker[F]
      xa <- HikariTransactor.newHikariTransactor[F](
        driverClassName = DbConfig.dbDriverName,
        url = DbConfig.dbUrl,
        user = DbConfig.dbUser,
        pass = DbConfig.dbPwd,
        connectEC = ce,
        blocker = be
      )
    } yield xa
}
