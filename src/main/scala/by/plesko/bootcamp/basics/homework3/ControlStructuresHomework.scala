package by.plesko.bootcamp.basics.homework3

import scala.Option
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructuresHomework {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result

  final case class CommandResult(command: Command, value: Double) extends Result

  val DivideCommand = "divide"
  val SumCommand = "sum"
  val AverageCommand = "average"
  val MinCommand = "min"
  val MaxCommand = "max"
  val ErrorPrefix = "Error:"

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val command :: values = x.split("\\p{Blank}+").filter(_.nonEmpty).toList
    val parsedValues = values.map(num => Try(num.toDouble).toEither)
    (command, parsedValues) match {
      case (null, _)                                                 => Left(ErrorMessage(s"$ErrorPrefix Empty line."))
      case (_, parsedVal) if parsedVal.exists(_.isLeft)              => Left(ErrorMessage(s"$ErrorPrefix Invalid arguments."))
      case (_, Nil)                                                  => Left(ErrorMessage(s"$ErrorPrefix Not enough arguments."))
      case (DivideCommand, Right(dividend) :: Right(divisor) :: Nil) => Right(Command.Divide(dividend, divisor))
      case (DivideCommand, xs) if xs.length > 2                      => Left(ErrorMessage(s"$ErrorPrefix Too many arguments."))
      case (SumCommand, xs)                                          => Right(Command.Sum(xs.flatMap(_.toSeq)))
      case (AverageCommand, xs)                                      => Right(Command.Average(xs.flatMap(_.toSeq)))
      case (MinCommand, xs)                                          => Right(Command.Min(xs.flatMap(_.toSeq)))
      case (MaxCommand, xs)                                          => Right(Command.Max(xs.flatMap(_.toSeq)))
      case _                                                         => Left(ErrorMessage(s"$ErrorPrefix Unknown command."))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Command.Divide(_, divisor) if divisor == 0d => Left(ErrorMessage(s"$ErrorPrefix Divisor is zero."))
      case Command.Divide(dividend, divisor)           => Right(CommandResult(x, dividend / divisor))
      case Command.Sum(numbers)                        => Right(CommandResult(x, numbers.sum))
      case Command.Average(numbers)                    => Right(CommandResult(x, numbers.sum / numbers.length))
      case Command.Min(numbers)                        => Right(CommandResult(x, numbers.min))
      case Command.Max(numbers)                        => Right(CommandResult(x, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case CommandResult(command: Command.Divide, value)  => s"${command.dividend} divided by ${command.divisor} is $value"
      case CommandResult(command: Command.Sum, value)     => s"the sum of ${command.numbers.mkString(" ")} is $value"
      case CommandResult(command: Command.Average, value) => s"the average of ${command.numbers.mkString(" ")} is $value"
      case CommandResult(command: Command.Min, value)     => s"the minimum of ${command.numbers.mkString(" ")} is $value"
      case CommandResult(command: Command.Max, value)     => s"the maximum of ${command.numbers.mkString(" ")} is $value"
    }
  }

  def process(x: String): String = {
    import cats.implicits._

    val res = for {
      parsedCommand <- parseCommand(x)
      calc <- calculate(parsedCommand)
    } yield renderResult(calc)
    res.leftMap(_.value).merge
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
