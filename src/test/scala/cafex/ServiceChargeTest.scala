package cafex

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class ServiceChargeTest extends WordSpec with Matchers with PropertyChecks {
  val checkout = new Checkout

  import checkout.Item._
  import checkout.ServiceCharge._

  "serviceCharge type for all drinks " should {
    val drinksGen = Gen.listOf(Gen.oneOf(Cola, Coffee))
    "produce DrinksOnlyServiceCharge" in {
      forAll(drinksGen) { (items) => {

        val actual = checkout.ServiceCharge.serviceChargeFactory(items)

        if (items.isEmpty)
          actual should be(None)
        else
          actual should be(Some(DrinksOnlyServiceCharge))

      }
      }
    }
  }

  "serviceCharge type for all item with cold food but no hot food " should {
    val itemsGen = Gen.listOf(Gen.oneOf(Cola, Coffee, CheeseSandwich))
    "produce ColdFoodOnlyServiceCharge" in {
      forAll(itemsGen) { (items) => {
        whenever(items.contains(CheeseSandwich)) {
          val actual = checkout.ServiceCharge.serviceChargeFactory(items)
          if (items.isEmpty)
            actual should be(None)
          else
            actual should be(Some(ColdFoodOnlyServiceCharge))
        }
      }
      }
    }
  }
  "serviceCharge type for all item with  hot food " should {
    val itemsGen = Gen.listOf(Gen.oneOf(Cola, Coffee, CheeseSandwich, SteakSandwich))
    "produce HotFoodServiceCharge" in {
      forAll(itemsGen) { (items) => {
        whenever(items.contains(SteakSandwich)) {
          val actual = checkout.ServiceCharge.serviceChargeFactory(items)
          if (items.isEmpty)
            actual should be(None)
          else
            actual should be(Some(HotFoodServiceCharge))
        }
      }
      }
    }
  }

  "serviceChargeValue for DrinkOnlyServiceCharge" should {
    val totalGen = for {
      value <- Gen.choose[Double](0, 10000)
      valueDecimal = BigDecimal(value.toString)
    } yield valueDecimal

    "produce zero charge" in {
      forAll(totalGen) { (total) => {
        val actual = checkout.ServiceCharge.serviceChargeValue(total, DrinksOnlyServiceCharge)
        actual should be(checkout.ZeroPrice)
      }
      }
    }
  }
  "serviceChargeValue for ColdFoodOnlyServiceCharge" should {
    val totalGen = for {
      value <- Gen.choose[Double](0, 10000)
      valueDecimal = BigDecimal(value.toString).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    } yield valueDecimal

    "produce value which is 10% of total" in {
      forAll(totalGen) { (total) => {
        val actual = checkout.ServiceCharge.serviceChargeValue(total, ColdFoodOnlyServiceCharge)
        val expected = (total * BigDecimal("0.10")).setScale(2, BigDecimal.RoundingMode.HALF_UP)
        actual should be(expected)
      }
      }
    }
  }
  "serviceChargeValue for HotFoodServiceCharge " should {
    val totalGen = for {
      value <- Gen.choose[Double](0, 10000)
      valueDecimal = BigDecimal(value.toString).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    } yield valueDecimal


    "produce value which is 20% of total capped by 20 pounds" in {
      forAll(totalGen) { (total) => {
        val actual = checkout.ServiceCharge.serviceChargeValue(total, HotFoodServiceCharge)
        val expected = (total * BigDecimal("0.20")).setScale(2, BigDecimal.RoundingMode.HALF_UP)
        if (expected < BigDecimal("20.00"))
          actual should be(expected)
        else
          actual should be(BigDecimal("20.0"))
      }
      }
    }
  }
}
