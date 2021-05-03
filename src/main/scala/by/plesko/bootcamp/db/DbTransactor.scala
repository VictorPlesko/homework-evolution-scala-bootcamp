package by.plesko.bootcamp.db

import cats.effect.{Blocker, ContextShift, Resource, Async}
import doobie.hikari.HikariTransactor
import doobie.{ExecutionContexts, Transactor}

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
