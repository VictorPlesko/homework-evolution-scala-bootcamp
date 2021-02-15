package by.plesko.bootcamp.adt

sealed trait Hand
object Hand {
  sealed abstract case class TexasHoldem private(cards: List[Card]) extends Hand
  object TexasHoldem {
    def create(cards: List[Card]): Either[Error, TexasHoldem] =
      Either.cond(cards.length == 4, new TexasHoldem(cards) {}, Error.InvalidSize("Invalid size of the list of cards in hand."))
  }

  sealed abstract case class OmahaHoldem private(cards: List[Card]) extends Hand
  object OmahaHoldem {
    def create(cards: List[Card]): Either[Error, OmahaHoldem] =
      Either.cond(cards.length == 5, new OmahaHoldem(cards) {}, Error.InvalidSize("Invalid size of the list of cards in hand."))
  }
}
