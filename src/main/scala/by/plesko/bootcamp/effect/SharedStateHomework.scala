package by.plesko.bootcamp.effect

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.syntax.all._
import cats.effect.syntax.all._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad : Timer : Concurrent, K, V](
                                                                   state: Ref[F, Map[K, (Long, V)]],
                                                                   expiresIn: FiniteDuration
                                                                 ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = {
      state
        .get
        .map {
          _.get(key)
            .map { case (_, value) => value }
        }
    }

    def put(key: K, value: V): F[Unit] = {
      state.update { oldValue =>
        oldValue + (key -> (expiresIn.toMillis, value))
      }
    }

    def timeoutCheck(
                      checkOnExpirationsEvery: FiniteDuration
                    ): F[Unit] = {
      for {
        _ <- Timer[F].sleep(checkOnExpirationsEvery)
        _ <- cleaningCacheByTime(checkOnExpirationsEvery)
      } yield ()
    }

    private def cleaningCacheByTime(checkOnExpirationsEvery: FiniteDuration): F[Unit] = {
      state.update {
        oldMap =>
          oldMap.map {
            case (k, (oldTime, v)) => k -> (oldTime - checkOnExpirationsEvery.toMillis, v)
          }.filter {
            case (_, (time, _)) => time > 0
          }
      }
    }
  }

  object Cache {
    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        refCache <- Ref.of(Map.empty[K, (Long, V)]).map(ref => new RefCache[F, K, V](ref, expiresIn))
        _ <- refCache.timeoutCheck(checkOnExpirationsEvery).foreverM.void.start
      } yield refCache

    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](12.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

