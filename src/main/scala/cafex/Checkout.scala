package cafex

import scalaz._
import std.option._, std.list._

class Checkout {

  sealed trait Item {
    val name: String
    val price: BigDecimal
  }

  object Item {

    private object Cola extends Item {
      override val name: String =  "Cola"
      override val price = BigDecimal("0.5")
    }

    private object Coffee extends Item {
      override val name: String = "Coffee"
      override val price: BigDecimal = BigDecimal("1.0")
    }

    private object CheeseSandwich extends Item {
      override val name: String = "Cheese Sandwich"
      override val price: BigDecimal =  BigDecimal("2.0")
    }

    private object SteakSandwich extends Item {
      override val name: String = "Steak Sandwich"
      override val price: BigDecimal = BigDecimal("4.5")
    }

    def maybeGetItem(str: String) = str match {
      case Cola.name => Some(Cola)
      case Coffee.name => Some(Coffee)
      case CheeseSandwich.name => Some(CheeseSandwich)
      case SteakSandwich.name => Some(SteakSandwich)
      case _ => None
    }
  }

  val ZeroPrice = BigDecimal("0.0")

  def totalBill(items: List[String]) =
    Traverse[List].traverse(items)(Item.maybeGetItem).map(_.map(_.price).sum)

}
