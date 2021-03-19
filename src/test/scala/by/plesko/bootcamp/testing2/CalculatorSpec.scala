package by.plesko.bootcamp.testing2

import by.plesko.bootcamp.testing2.Error._
import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {

  test("enter a number in the range from 0 to 9") {
    val calculator = Calculator()
    assert(calculator.enter(0) == Right(Calculator(0, 0, None)))
    assert(calculator.enter(5) == Right(Calculator(0, 5, None)))
    assert(calculator.enter(9) == Right(Calculator(0, 9, None)))
  }

  test("out-of-range number input") {
    val calculator = Calculator()
    assert(calculator.enter(10) == Left(DigitOutOfRange))
    assert(calculator.enter(-1) == Left(DigitOutOfRange))
  }

  test("correct operators") {
    val calculator = Calculator(screen = 1)
    assert(calculator.plus.operation.contains(Calculator.Operation.Plus))
    assert(calculator.minus.operation.contains(Calculator.Operation.Minus))
    assert(calculator.multiply.operation.contains(Calculator.Operation.Multiply))
    assert(calculator.divide.operation.contains(Calculator.Operation.Divide))
    assert(calculator.cleanScreen == Calculator())
  }

  test("correct calculations") {
    val calculatorPlus = Calculator(memory = 10, screen = 10, Some(Calculator.Operation.Plus))
    val calculatorMinus = Calculator(memory = 10, screen = 10, Some(Calculator.Operation.Minus))
    val calculatorMultiply = Calculator(memory = 10, screen = 10, Some(Calculator.Operation.Multiply))
    val calculatorDivide = Calculator(memory = 10, screen = 10, Some(Calculator.Operation.Divide))
    val calculatorDivideByZero = Calculator(memory = 10, screen = 0, Some(Calculator.Operation.Divide))
    val calculatorNoOperation = Calculator(memory = 10, screen = 0, None)
    assert(calculatorPlus.calculate == Right(Calculator(memory = 20)))
    assert(calculatorMinus.calculate == Right(Calculator(memory = 0)))
    assert(calculatorMultiply.calculate == Right(Calculator(memory = 100)))
    assert(calculatorDivide.calculate == Right(Calculator(memory = 1)))
    assert(calculatorDivideByZero.calculate == Left(DivisionByZero))
    assert(calculatorNoOperation.calculate == Right(Calculator(memory = 10)))
  }

  test("checking error messages"){
    assert(DigitOutOfRange.getMessage == "Digit out of range")
    assert(DivisionByZero.getMessage == "You cannot divide by zero!")
  }
}
