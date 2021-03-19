package by.plesko.bootcamp.testing2

import Calculator._
import Error._

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */

sealed trait Error {
  def getMessage: String
}

object Error {
  final case object DigitOutOfRange extends Error {
    override def getMessage: String = "Digit out of range"
  }

  final case object DivisionByZero extends Error {
    override def getMessage: String = "You cannot divide by zero!"
  }
}

case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None) {

  def enter(digit: Int): Either[Error, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(this.copy(screen = screen * 10 + digit))
    } else {
      Left(DigitOutOfRange)
    }

  def plus: Calculator = this.copy(operation = Some(Operation.Plus))

  def minus: Calculator = this.copy(operation = Some(Operation.Minus))

  def multiply: Calculator = this.copy(operation = Some(Operation.Multiply))

  def divide: Calculator = this.copy(operation = Some(Operation.Divide))

  def cleanScreen: Calculator = this.copy(screen = 0)

  def calculate: Either[Error, Calculator] = operation.fold(Right(this): Either[Error, Calculator]) {
    case Operation.Plus => Right(Calculator(memory = screen + memory))
    case Operation.Minus => Right(Calculator(memory = screen - memory))
    case Operation.Multiply => Right(Calculator(memory = screen * memory))
    case Operation.Divide if screen == 0 => Left(DivisionByZero)
    case Operation.Divide => Right(Calculator(memory = memory / screen))
  }

}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
    object CleanScreen extends Operation
  }
}