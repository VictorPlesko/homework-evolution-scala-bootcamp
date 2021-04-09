package by.plesko.bootcamp.effect

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import cats.effect.concurrent.Ref
import cats.Functor
import cats.implicits._

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source, StdIn}

object EffectsHomework2 extends IOApp {

  sealed trait FileError {
    def toString: String
  }

  object FileError {
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

  def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO]): IO[Int] =
    blocker
      .blockOn(IO(println("Enter seed: ")) *> IO(StdIn.readInt()))
      .handleErrorWith(_ => IO(println("Not a number!")) *> readSeed(blocker))

  private def checkFilePath(filePath: String): Either[FileError, Unit] = {
    val file = new File(filePath)
    if (!file.exists) Left(FileError.NotExists)
    else if (!file.isFile) Left(FileError.NotFile)
    else if (!file.canRead) Left(FileError.NotReadable)
    else Right()
  }

  object Hash {
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
  }

  trait Repo[F[_]] {
    def add(filePath: String, minHash: Int): F[Unit]
    def get(filePath: String): F[Int]
  }

  final case class MyRepo[F[_] : Functor](
                                           private val repo: Ref[F, Map[String, Int]]
                                         ) extends Repo[F] {
    override def add(filePath: String, minHash: Int): F[Unit] = repo.update(_ + (filePath -> minHash))

    override def get(filePath: String): F[Int] = repo.get.map(_ (filePath))
  }

  object MyRepo {
    def emptyRepo[F[_] : Sync]: F[MyRepo[F]] =
      Ref.of[F, Map[String, Int]](Map.empty[String, Int]).map(ref => MyRepo(ref))
  }

  def calculateSignature(filePath: String, descriptor: BufferedSource, seed: Int): IO[Unit] = {
    for {
      data <- IO(descriptor.getLines().toList)
      words = data.flatMap(_.split("[\\p{Space}\\p{Punct}]").filter(_ != ""))
      javaHashFiber <- IO.pure(words.map(word => Hash.javaHash(word, seed)).min).start
      knuthHashFiber <- IO.pure(words.map(word => Hash.knuthHash(word, seed)).min).start
      javaMinHash <- javaHashFiber.join
      knuthMinHash <- knuthHashFiber.join
      minHash = javaMinHash min knuthMinHash
      myRepo <- MyRepo.emptyRepo[IO]
      _ <- myRepo.add(filePath, minHash)
      test <- myRepo.get(filePath)
      _ <- IO(println(s"FilePath: $filePath, MinHash: $test"))
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val executors = Executors.newFixedThreadPool(3)
    val executionContext = ExecutionContext.fromExecutor(executors)
    val blocker = Blocker.liftExecutionContext(executionContext)

    val resources = for {
      filePath <- Resource.liftF(readFilePath(blocker))
      seed <- Resource.liftF(readSeed(blocker))
      descriptor <- Resource.fromAutoCloseable(IO(Source.fromFile(filePath)))
    } yield (filePath, descriptor, seed)

    resources.use {
      case (filePath, descriptor, seed) =>
        contextShift.evalOn(executionContext)(calculateSignature(filePath, descriptor, seed))
    } *> IO(executors.shutdown()) *> IO(ExitCode.Success)
  }
}
