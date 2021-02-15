package by.plesko.bootcamp.adt

sealed abstract case class Board private(cards: List[Card])
object Board {
  def create(cards: List[Card]): Either[Error, Board] =
    Either.cond(cards.length == 5, new Board(cards) {}, Error.InvalidSize("Invalid size of the list of cards on the board."))
}
