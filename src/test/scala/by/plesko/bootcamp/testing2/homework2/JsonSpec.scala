package by.plesko.bootcamp.testing2.homework2

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters


class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  import Json._

  implicit val params = Parameters.default.withMinSuccessfulTests(1000)

  def jNullGen: Gen[JNull.type] = Gen.const(JNull)

  def jBooleanGen: Gen[JBoolean] = Gen.oneOf(true, false).map(JBoolean)

  def jNumberGen: Gen[JNumber] = Gen.double.map(JNumber)

  def jStringGen: Gen[JString] = Gen.alphaStr.map(JString)

  def jArrayGen(maxSize: Int): Gen[JArray] = {
    if (maxSize == 0)
      oneOfSimpleJson.map(json => JArray(Vector(json)))
    else
      Gen.choose(1, maxSize).flatMap(size => Gen.listOfN(size, oneOfAllJson(maxSize - 1)).map(arr => JArray(arr.toVector)))
  }

  def jObject(maxSize: Int): Gen[JObject] = {
    if (maxSize == 0)
      Gen.alphaStr.flatMap(keyValue => oneOfSimpleJson.map(json => Map(keyValue -> json))).map(JObject)
    else
      for {
        size <- Gen.choose(1, maxSize)
        listJson <- Gen.listOfN(size, oneOfAllJson(maxSize - 1))
        keyName <- Gen.alphaStr
        mapVal = listJson.map(json => (keyName, json)).toMap
      } yield JObject(mapVal)
  }

  def oneOfAllJson(maxSize: Int): Gen[Json] = Gen.oneOf(jNullGen, jBooleanGen, jNumberGen, jStringGen, jArrayGen(maxSize), jObject(maxSize))

  def oneOfSimpleJson: Gen[Json] = Gen.oneOf(jNullGen, jBooleanGen, jNumberGen, jStringGen)

  def jsonGen: Gen[Json] = Gen.sized(size => jObject(Math.sqrt(size).round.toInt))

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      assert(parse(print(json)) == Some(json))
    }
  }
}
