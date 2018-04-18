package cafex

import scalaz._
import scalaz.std.list._
import scalaz.std.option._

class Checkout {

  sealed trait Item {
    val name: String
    val price: BigDecimal
    val isFood: Boolean
    val isHot: Boolean
  }

  object Item {

    object Cola extends Item {
      override val name: String = "Cola"
      override val price = BigDecimal("0.5")
      override val isFood: Boolean = false
      override val isHot: Boolean = false
    }

    object Coffee extends Item {
      override val name: String = "Coffee"
      override val price: BigDecimal = BigDecimal("1.0")
      override val isFood: Boolean = false
      override val isHot: Boolean = true
    }

    object CheeseSandwich extends Item {
      override val name: String = "Cheese Sandwich"
      override val price: BigDecimal = BigDecimal("2.0")
      override val isFood: Boolean = true
      override val isHot: Boolean = false
    }

    object SteakSandwich extends Item {
      override val name: String = "Steak Sandwich"
      override val price: BigDecimal = BigDecimal("4.5")
      override val isFood: Boolean = true
      override val isHot: Boolean = true
    }

    def maybeGetItem(str: String): Option[Item] = str match {
      case Cola.name => Some(Cola)
      case Coffee.name => Some(Coffee)
      case CheeseSandwich.name => Some(CheeseSandwich)
      case SteakSandwich.name => Some(SteakSandwich)
      case _ => None
    }
  }

  sealed trait ServiceCharge {
    val charge = BigDecimal("0.0")
  }

  object ServiceCharge {

    case object DrinksOnlyServiceCharge extends ServiceCharge

    case object ColdFoodOnlyServiceCharge extends ServiceCharge {
      override val charge: BigDecimal = BigDecimal("10.0")
    }

    case object HotFoodServiceCharge extends ServiceCharge {
      override val charge: BigDecimal = BigDecimal("20.0")
      val capped = BigDecimal("20.0")
    }

    def serviceChargeFactory(items: List[Item]): Option[ServiceCharge] =
      items.foldLeft(false, false, false, false) { (acc, x) =>
        (acc._1 || (x.isFood && !x.isHot),
          acc._2 || (x.isFood && x.isHot),
          acc._3 || (!x.isFood && !x.isHot),
          acc._4 || (!x.isFood && x.isHot))
      } match {
        case (false, false, false, false) => None
        case (false, false, _, _) => Some(DrinksOnlyServiceCharge)
        case (true, false, _, _) => Some(ColdFoodOnlyServiceCharge)
        case (_, true, _, _) => Some(HotFoodServiceCharge)
      }


    def serviceChargeValue(total: BigDecimal, serviceCharge: ServiceCharge):BigDecimal =
      serviceCharge match {
        case HotFoodServiceCharge =>
          val charge = (total * HotFoodServiceCharge.charge / BigDecimal("100")).setScale(2, BigDecimal.RoundingMode.HALF_UP)
          if (charge >= HotFoodServiceCharge.capped) {
            HotFoodServiceCharge.capped
          } else {
            charge
          }
        case s: ServiceCharge => (total * s.charge / BigDecimal("100")).setScale(2, BigDecimal.RoundingMode.HALF_UP)
      }
    }

  val ZeroPrice = BigDecimal("0.0")

  def totalBill(items: List[String]): Option[BigDecimal] = {
    import ServiceCharge._
    for {
      itemObjects <- Traverse[List].traverse(items)(Item.maybeGetItem)
      serviceCharge <- serviceChargeFactory(itemObjects)
    } yield {
      val total = itemObjects.map(_.price).sum
      total + serviceChargeValue(total, serviceCharge)
    }
  }
}
