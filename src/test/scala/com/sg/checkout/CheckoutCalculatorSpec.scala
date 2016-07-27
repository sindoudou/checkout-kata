package com.sg.checkout

import com.example.com.sg.checkout.CheckoutCalculator
import org.scalatest._

class CheckoutCalculatorSpec extends FlatSpec with Matchers {
  val rulesMap = Map(
    "A" -> Map(1 -> 50, 3 -> 130),
    "B" -> Map(1 -> 30, 2 -> 45),
    "C" -> Map(1 -> 20),
    "D" -> Map(1 -> 15)
  )

  val checkout = new CheckoutCalculator(rulesMap)

  "CheckoutCalculator" should "return for 50 for A" in {
    checkout.calculate(List("A")) shouldBe 50
  }

  "CheckoutCalculator" should "return for 30 for B" in {
    checkout.calculate(List("B")) shouldBe 30
  }

  "CheckoutCalculator" should "return for 20 for C" in {
    checkout.calculate(List("C")) shouldBe 20
  }

  "CheckoutCalculator" should "return for 15 for D" in {
    checkout.calculate(List("D")) shouldBe 15
  }

  "CheckoutCalculator" should "return for 130 for A,A,A" in {
    checkout.calculate(List("A", "A", "A")) shouldBe 130
  }

  "CheckoutCalculator" should "return for 45 for B, B" in {
    checkout.calculate(List("B", "B")) shouldBe 45
  }

  "CheckoutCalculator" should "return for 95 for B, A, B" in {
    checkout.calculate(List("B", "A", "B")) shouldBe 95
  }

  "CheckoutCalculator" should "return for 125 for B, A, B, B" in {
    checkout.calculate(List("B", "A", "B", "B")) shouldBe 125
  }

  "CheckoutCalculator" should "return for 140 for B, A, B, B, B" in {
    checkout.calculate(List("B", "A", "B", "B", "B")) shouldBe 140
  }
}
