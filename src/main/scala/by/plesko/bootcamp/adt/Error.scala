package by.plesko.bootcamp.adt

sealed trait Error
object Error {
  final case class InvalidSize(message: String) extends Error
}
