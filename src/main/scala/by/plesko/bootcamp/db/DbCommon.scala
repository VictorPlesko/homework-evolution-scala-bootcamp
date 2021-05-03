package by.plesko.bootcamp.db

import doobie.{ConnectionIO, Fragment}

import java.util.UUID

object DbCommon {

  val authorOdersky: UUID = UUID.randomUUID()
  val authorRowling: UUID = UUID.randomUUID()
  val bookScala: UUID = UUID.randomUUID()
  val bookHPStone: UUID = UUID.randomUUID()
  val bookHPSecrets: UUID = UUID.randomUUID()

  val createTableAuthorsSql: String =
    """CREATE TABLE authors (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL,
      |  birthday DATE);""".stripMargin

  val createTableBooksSql: String =
    """CREATE TABLE books (
      |  id UUID PRIMARY KEY,
      |  author UUID NOT NULL,
      |  title VARCHAR(100) NOT NULL,
      |  genre VARCHAR(100) NOT NULL,
      |  year INT,
      |  FOREIGN KEY (author) REFERENCES authors(id));""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO authors (id, name, birthday) VALUES
       |  ('$authorOdersky', 'Martin Odersky', '1958-09-05'),
       |  ('$authorRowling', 'J.K. Rowling', '1965-07-31');
       |
       |INSERT INTO books (id, author, title, genre, year) VALUES
       |  ('$bookScala', '$authorOdersky', 'Programming in Scala', 'Programming', 2016),
       |  ('$bookHPStone', '$authorRowling', 'Harry Potter and Philosopher''s Stone','Fantasy', 1997),
       |  ('$bookHPSecrets', '$authorRowling', 'Harry Potter and the Chamber of Secrets','Fantasy', 1998);
       |""".stripMargin

  val ddl1: Fragment = Fragment.const(createTableAuthorsSql)
  val ddl2: Fragment = Fragment.const(createTableBooksSql)
  val dml: Fragment = Fragment.const(populateDataSql)

  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()
}
