package cafex

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

class CheckoutTest extends FeatureSpec with GivenWhenThen with Matchers {
  feature("The user can produce a bill for purchased items") {
    info("As a person responsible for producing a bill")
    info("I want to be able to charge Cafe-X customers")
    scenario("total bill of an empty purchased items list") {
      Given("an empty list of purchased items")
      val checkout = new Checkout()
      When("totalBill is invoked on that list")
      Then("produces None ")
      checkout.totalBill(Nil) should be(None)
    }

    scenario("total bill of only drinks") {
      Given("only drinks in the item list")
      val checkout = new Checkout()
      val items: List[String] = List("Cola", "Cola", "Coffee")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(2.0)")
      checkout.totalBill(items) should be(Some(BigDecimal("2.0")))
    }

    scenario("total bill of drinks and cold food") {
      Given("drinks and only cold food in th list")
      val checkout = new Checkout()
      val items: List[String] = List("Cola", "Cola", "Coffee", "Cheese Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(4.40) which 10% higher than totalBill without service charge")
      checkout.totalBill(items) should be(Some(BigDecimal("4.40")))
    }

    scenario("total bill for items with hot food and up to 20 pounds service charge") {
      Given("drinks, hot  and  cold food in the list")
      val checkout = new Checkout()
      val items: List[String] = List("Coffee", "Cheese Sandwich", "Steak Sandwich", "Steak Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(14.4) which 20% higher than totalBill without service charge")
      checkout.totalBill(items) should be(Some(BigDecimal("14.4")))
    }

    scenario("total bill for list with hot food and uncapped service chage is higher than 20 pounds ") {
      Given("hot food in the list")
      val checkout = new Checkout()
      val items: List[String] = List.fill(100)("Steak Sandwich")
      When("totalBill is invoked on that list")
      Then("the total bill is Some(470) which totalBill + 20 pounds")
      checkout.totalBill(items) should be(Some(BigDecimal("470.00")))
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
