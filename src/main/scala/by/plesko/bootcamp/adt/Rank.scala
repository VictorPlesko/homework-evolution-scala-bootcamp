package by.plesko.bootcamp.adt

sealed trait Rank
object Rank {
  final case class Ace(weight: Int = 14) extends Rank
  final case class King(weight: Int = 13) extends Rank
  final case class Queen(weight: Int = 12) extends Rank
  final case class Jack(weight: Int = 11) extends Rank
  final case class Ten(weight: Int = 10) extends Rank
  final case class Nine(weight: Int = 9) extends Rank
  final case class Eight(weight: Int = 8) extends Rank
  final case class Seven(weight: Int = 7) extends Rank
  final case class Six(weight: Int = 6) extends Rank
  final case class Five(weight: Int = 5) extends Rank
  final case class Four(weight: Int = 4) extends Rank
  final case class Three(weight: Int = 3) extends Rank
  final case class Two(weight: Int = 2) extends Rank
}
