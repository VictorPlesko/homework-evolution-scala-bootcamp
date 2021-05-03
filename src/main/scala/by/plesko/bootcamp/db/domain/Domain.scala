package by.plesko.bootcamp.db.domain

import io.circe.generic.JsonCodec

import java.time.{LocalDate, Year}
import java.util.UUID

@JsonCodec
final case class Author(id: UUID, name: String, birthday: LocalDate)

@JsonCodec
final case class AuthorWithoutID(name: String, birthday: LocalDate)

@JsonCodec
final case class Book(id: UUID, authorId: UUID, title: String, genre: String, year: Year)

@JsonCodec
final case class BookWithoutID(authorId: UUID, title: String, genre: String, year: Year)

@JsonCodec
final case class BookWithAuthor(id: UUID, author: Author, title: String, genre: String, year: Year)




