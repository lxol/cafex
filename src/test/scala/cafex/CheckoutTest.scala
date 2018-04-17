package cafex

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

class CheckoutTest extends FeatureSpec with GivenWhenThen with Matchers {
  feature("The user can produce a bill for purchased items") {
    info("As a person responsible for producing a bill")
    info("I want to be able to charge Cafe-X customers")
    scenario("total bill of an empty purchased items list") {
      Given("an empty list of purchased items")
      val checkout = new Checkout()
      val items: List[String] = Nil
      When("totalBill is invoked on that list")
      Then("the total bill is Some(0.0)")
      checkout.totalBill(Nil) should be(Some(BigDecimal(0.00)))
    }

    scenario("total bill of non-empty valid items without duplicates") {
      Given("valid items without duplications")
      val checkout = new Checkout()
      val items: List[String] = List("Cola", "Coffee", "Cheese Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(3.5)")
      checkout.totalBill(items) should be(Some(BigDecimal("3.5")))
    }

    scenario("total bill of non-empty valid items with duplicates") {
      Given("valid items without duplications")
      val checkout = new Checkout()
      val items: List[String] = List("Steak Sandwich", "Steak Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(9.0)")
      checkout.totalBill(items) should be(Some(BigDecimal("9.0")))
    }

    scenario("total bill of list with invalid item") {
      Given("list with one invalid item")
      val checkout = new Checkout()
      val items: List[String] = List("Cola", "Invalid", "Cheese Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is None")
      checkout.totalBill(items) should be(None)
    }
  }
}
