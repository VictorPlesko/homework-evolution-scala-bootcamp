package by.plesko.bootcamp.adt

final case class Result(hands: List[Hand]) extends AnyVal
final case class TestResult(sortedHands: Result)
