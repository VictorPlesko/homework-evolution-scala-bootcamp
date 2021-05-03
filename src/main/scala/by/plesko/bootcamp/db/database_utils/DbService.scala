package by.plesko.bootcamp.db.database_utils

import by.plesko.bootcamp.db.domain._
import cats.effect.Effect
import cats.implicits._
import doobie.implicits._
import doobie.implicits.javatime._
import AdditionalMeta._
import doobie.{ConnectionIO, Fragment, Meta, Transactor}

import java.time.Year
import java.util.UUID

object AdditionalMeta {
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
}

object SQLCommand {

  def insertAuthor(author: Author): ConnectionIO[Int] = {
    val sql = sql"INSERT INTO authors (id, `name`, birthday) VALUES(${author.id}, ${author.name}, ${author.birthday})"
    sql.update.run
  }

  def insertBook(book: Book): ConnectionIO[Int] = {
    val sql = sql"INSERT INTO books (id, author, title, genre, `year`) VALUES(${book.id}, ${book.authorId}, ${book.title}, ${book.genre}, ${book.year})"
    sql.update.run
  }

  private val sqlFragmentForReadBooks: Fragment =
    fr"SELECT b.id, a.id, a.name, a.birthday, b.title, b.genre, b.year FROM books b INNER JOIN authors a ON b.author = a.id"

  def readAllBooks: ConnectionIO[List[BookWithAuthor]] =
    sqlFragmentForReadBooks.query[BookWithAuthor].to[List]

  def readBook(uuid: UUID): ConnectionIO[Option[BookWithAuthor]] = {
    val sql = sqlFragmentForReadBooks ++ fr"WHERE b.id = $uuid"
    sql.query[BookWithAuthor].option
  }

  private val sqlFragmentForReadAuthor: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  def readAllAuthors: ConnectionIO[List[Author]] =
    sqlFragmentForReadAuthor.query[Author].to[List]

  def readAuthor(uuid: UUID): ConnectionIO[Option[Author]] = {
    val sql = sqlFragmentForReadAuthor ++ fr"WHERE id = $uuid"
    sql.query[Author].option
  }

  def updateBook(uuid: UUID, book: BookWithoutID): ConnectionIO[Int] = {
    sql"UPDATE books SET author = ${book.authorId}, title = ${book.title}, genre = ${book.genre}, year = ${book.year} WHERE id = $uuid".update.run
  }

  def updateAuthor(uuid: UUID, author: AuthorWithoutID): ConnectionIO[Int] = {
    sql"UPDATE authors SET `name` = ${author.name}, birthday = ${author.birthday} WHERE id = $uuid".update.run
  }

  private val sqlFragmentForDeleteAuthor = fr"DELETE FROM authors"
  private val sqlFragmentForDeleteBook = fr"DELETE FROM books"

  def deleteAuthor(uuid: UUID): ConnectionIO[Int] = {
    val sql = sqlFragmentForDeleteAuthor ++ fr"WHERE id = $uuid"
    sql.update.run
  }

  def deleteBook(uuid: UUID): ConnectionIO[Int] = {
    val sql = sqlFragmentForDeleteBook ++ fr"WHERE id = $uuid"
    sql.update.run
  }
}

final case class DbService[F[_] : Effect](private val xa: Transactor[F]) {

  def insertBook(book: Book): F[Unit] = SQLCommand.insertBook(book).transact(xa).void

  def insertAuthor(author: Author): F[Unit] = SQLCommand.insertAuthor(author).transact(xa).void

  def readAllBooks: F[List[BookWithAuthor]] = SQLCommand.readAllBooks.transact(xa)

  def readBook(uuid: UUID): F[Option[BookWithAuthor]] = SQLCommand.readBook(uuid).transact(xa)

  def readAllAuthors: F[List[Author]] = SQLCommand.readAllAuthors.transact(xa)

  def readAuthor(uuid: UUID): F[Option[Author]] = SQLCommand.readAuthor(uuid).transact(xa)

  def updateBook(uuid: UUID, book: BookWithoutID): F[Option[Unit]] =
    SQLCommand.updateBook(uuid, book).transact(xa).map(rows => if (rows == 0) None else Some(()))

  def updateAuthor(uuid: UUID, author: AuthorWithoutID): F[Option[Unit]] =
    SQLCommand.updateAuthor(uuid, author).transact(xa).map(rows => if (rows == 0) None else Some(()))

  def deleteAuthor(uuid: UUID): F[Option[Unit]] =
    SQLCommand.deleteAuthor(uuid).transact(xa).map[Option[Unit]](rows => if (rows == 0) None else Some(()))

  def deleteBook(uuid: UUID): F[Option[Unit]] =
    SQLCommand.deleteBook(uuid).transact(xa).map[Option[Unit]](rows => if (rows == 0) None else Some(()))
}