package by.plesko.bootcamp.adt

sealed trait PokerCombination
object PokerCombination {
  final case class StraightFlush(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class FourOfKind(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class FullHouse(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class Flush(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class Straight(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class ThreeOfKind(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class TwoPairs(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class Pair(combination: List[Card], residue: List[Card]) extends PokerCombination
  final case class HighCard(combination: List[Card], residue: List[Card]) extends PokerCombination
}
