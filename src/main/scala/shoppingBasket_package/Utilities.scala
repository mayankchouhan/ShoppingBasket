import shoppingBasket_package._

object CreateObject_DP_Factory {
  import shoppingBasket_package.Products._
  def getItem(name: String): Either[String, Item] = name.toLowerCase match {
    case SOUP => Right(Soup())
    case BREAD => Right(Bread())
    case MILK => Right(Milk())
    case APPLES => Right(Apple())
    case exc => Left("Incorrect Input Item " + exc)
  }
}

