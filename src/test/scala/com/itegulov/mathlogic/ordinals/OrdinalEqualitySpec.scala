package com.itegulov.mathlogic.ordinals

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
class OrdinalEqualitySpec extends FlatSpec with Matchers {

  def test(filename: String): Boolean =
    new OrdinalParser(Source.fromFile(filename, "UTF-8").mkString).equalityOrdinals.run().get match {
      case (x, y) => x.convertToCNF == y.convertToCNF
    }

  "Ordinals" should "be equal" in {
    test("src/main/resources/equal1.in") shouldBe true
    test("src/main/resources/equal2.in") shouldBe true
    test("src/main/resources/equal3.in") shouldBe true
    test("src/main/resources/equal4.in") shouldBe true
    test("src/main/resources/equal5.in") shouldBe true
    test("src/main/resources/equal6.in") shouldBe true
    test("src/main/resources/equal7.in") shouldBe true
  }

  "Ordinals" should "not be equal" in {
    test("src/main/resources/different1.in") shouldBe false
    //test("src/main/resources/different2.in") shouldBe false
    test("src/main/resources/different3.in") shouldBe false
    test("src/main/resources/different4.in") shouldBe false
    test("src/main/resources/different5.in") shouldBe false
    test("src/main/resources/different6.in") shouldBe false
    test("src/main/resources/different7.in") shouldBe false
  }
}
