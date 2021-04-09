package by.plesko.bootcamp.effect

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import cats.effect.concurrent.Ref
import cats.Functor
import cats.implicits._

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.{Source, StdIn}

object EffectsHomework2 extends IOApp {

  sealed trait FileError {
    def toString: String
  }

  object Error {
    final case object NotFile extends FileError {
      override def toString: String = "The file path does not point to a file!"
    }
    final case object NotReadable extends FileError {
      override def toString: String = "The file cannot be read!"
    }
    final case object NotExists extends FileError {
      override def toString: String = "The file does not exist at the given path!"
    }
  }

  def readFilePath(blocker: Blocker)(implicit contextShift: ContextShift[IO]): IO[String] = {
    blocker.blockOn {
      (for {
        _ <- IO(println("Enter file path: "))
        path <- IO(StdIn.readLine())
      } yield checkFilePath(path) match {
        case Left(value) => IO(println(value)) *> readFilePath(blocker)
        case Right(_) => IO(path)
      }).flatten
    }
  }

  private def checkFilePath(filePath: String): Either[FileError, Unit] = {
    val file = new File(filePath)
    if (!file.exists) Left(Error.NotExists)
    else if (!file.isFile) Left(Error.NotFile)
    else if (!file.canRead) Left(Error.NotReadable)
    else Right()
  }

  def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO]): IO[Int] =
    blocker
      .blockOn(IO(println("Enter seed: ")) *> IO(StdIn.readInt()))
      .handleErrorWith(_ => IO(println("Not a number!")) *> readSeed(blocker))

  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0

    for (ch <- word.toCharArray)
      hash = 31 * seed * hash + ch.toInt

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }

  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  trait Repo[F[_]] {
    def add(filePath: String, minHash: Int): F[Unit]
    def get(filePath: String): F[Int]
  }
  final case class MyRepo[F[_]: Functor](
  private val repo: Ref[F, Map[String, Int]]
  ) extends Repo[F] {
    override def add(filePath: String, minHash: Int): F[Unit] = repo.update(_ + (filePath -> minHash))

    override def get(filePath: String): F[Int] = repo.get.map(_(filePath))
  }
  object MyRepo {
    def emptyRepo[F[_]: Sync]: F[MyRepo[F]] =
      Ref.of[F,Map[String,Int]](Map.empty[String, Int]).map(ref => MyRepo(ref))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val resources = for {
      executor <- Resource.make(IO(Executors.newFixedThreadPool(3)))(ex => IO(ex.shutdown()))
      blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(executor))
      filePath <- Resource.liftF(readFilePath(blocker))
      seed <- Resource.liftF(readSeed(blocker))
      descriptor <- Resource.fromAutoCloseable(IO(Source.fromFile(filePath)))
    } yield (filePath, descriptor, seed)

    resources.use {
      case (filePath, descriptor, seed) =>
          for {
            data <- IO(descriptor.getLines().toList)
            words = data.flatMap(_.split("[\\p{Space}\\p{Punct}]").filter(_ != ""))
            _ <- IO(println(words))
            javaHashFiber <- IO.pure(words.map(word => javaHash(word, seed)).min).start
            knuthHashFiber <- IO.pure(words.map(word => knuthHash(word, seed)).min).start
            javaMinHash <- javaHashFiber.join
            knuthMinHash <- knuthHashFiber.join
            _ <- IO(println(s"java: $javaMinHash, knuth: $knuthMinHash"))
            minHash = javaMinHash min knuthMinHash
            myRepo <- MyRepo.emptyRepo[IO]
            _ <- myRepo.add(filePath, minHash)
            test <- myRepo.get(filePath)
            _ <- IO(println(s"FilePath: $filePath, MinHash: $minHash"))
          } yield ()

    } as ExitCode.Success
  }
}
