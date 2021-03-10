package by.plesko.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

import java.util.Calendar
import scala.util.{Success, Try}

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

object Homework {

  object entity {
    final case class ExpirationDate(month: Int, year: Int)
    final case class Owner(name: String, surname: String)
    final case class CardNumber(number: String) extends AnyVal
    final case class SecurityCode(code: String) extends AnyVal
  }

  import entity._

  case class PaymentCard(name: Owner, numberCard: CardNumber, expirationDate: ExpirationDate, securityCode: SecurityCode)

  sealed trait ValidationError
  object ValidationError {
    final case object NotANumeric extends ValidationError {
      override def toString: String = "There must be a number."
    }
    final case object EmptyLine extends ValidationError {
      override def toString: String = "The string must not be empty "
    }
    final case object CardNumberIsOutOfBounds extends ValidationError {
      override def toString: String = "Card number must be 16 digits."
    }
    final case object SecurityCodeIsOutOfBounds extends ValidationError {
      override def toString: String = "Security code must be 3 digits."
    }
    final case object OwnerParameterIsOutOfBounds extends ValidationError {
      override def toString: String = "Owner's name consists of first and last name."
    }
    final case object OwnerHasSpecialCharacters extends ValidationError {
      override def toString: String = "Name or surname cannot contain special characters."
    }
    final case object MonthIsOutOfBounds extends ValidationError {
      override def toString: String = "Month of validity of the card must be from 1 to 12."
    }
    final case object YearIsOutOfBounds extends ValidationError {
      override def toString: String = "Card validity year must be greater than or equal to the specified year."
    }
    final case object InvalidSizeExpirationDate extends ValidationError {
      override def toString: String = "The date must be a month and a year (MM/YY)."
    }
  }

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    import ValidationError._

    private def validateOwner(name: String): AllErrorsOr[Owner] = {

      def validateOwnerSymbols(nameOwner: String, surnameOwner:String): AllErrorsOr[Owner] = {
        if (name.matches("^[a-zA-Z ]+$")) Owner(nameOwner, surnameOwner).validNec
        else OwnerHasSpecialCharacters.invalidNec
      }

      if (name.isEmpty) EmptyLine.invalidNec
      else {
        val splitName = name.split("\\s+").map(_.toUpperCase)
        if (splitName.length != 2) OwnerParameterIsOutOfBounds.invalidNec
        else validateOwnerSymbols(splitName(0), splitName(1))
      }
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {

      def validateCardNumberIsNumeric: AllErrorsOr[CardNumber] = {
        if (number.forall(_.isDigit)) CardNumber(number).validNec
        else NotANumeric.invalidNec
      }

      def validateCardNumberLength: AllErrorsOr[CardNumber] = {
        if (number.length == 16) CardNumber(number).validNec
        else CardNumberIsOutOfBounds.invalidNec
      }

      if (number.isEmpty) EmptyLine.invalidNec
      else {
        validateCardNumberIsNumeric.productR(validateCardNumberLength)
      }
    }

    private def validateExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {

      def validateMonth(month: Int, year: Int): AllErrorsOr[ExpirationDate] = {
        if (month >= 1 && month <= 12) ExpirationDate(month, year).validNec
        else MonthIsOutOfBounds.invalidNec
      }

      def validateYear(month: Int, year: Int): AllErrorsOr[ExpirationDate] = {
        if (year >= Calendar.getInstance().get(Calendar.YEAR) % 100 && year <= 99) ExpirationDate(month, year).validNec
        else YearIsOutOfBounds.invalidNec
      }

      if (date.isEmpty) EmptyLine.invalidNec
      else {
        val splitDate = date.split("/")
        if (splitDate.length != 2) InvalidSizeExpirationDate.invalidNec
        else {
          (splitDate(0).toIntOption, splitDate(1).toIntOption) match {
            case (Some(month), Some(year)) =>validateMonth(month, year).productR(validateYear(month, year))
            case (_, _) => NotANumeric.invalidNec
          }
        }
      }
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] = {

      def validateCodeLength: AllErrorsOr[SecurityCode] = {
        if (securityCode.length == 3) SecurityCode(securityCode).validNec
        else SecurityCodeIsOutOfBounds.invalidNec
      }

      def validateCodeIsNumeric: AllErrorsOr[SecurityCode] = {
        if (securityCode.forall(_.isDigit)) SecurityCode(securityCode).validNec
        else NotANumeric.invalidNec
      }

      if (securityCode.isEmpty) EmptyLine.invalidNec
      else {
        validateCodeLength.productR(validateCodeIsNumeric)
      }
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (validateOwner(name), validateCardNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN(PaymentCard)
  }
}
