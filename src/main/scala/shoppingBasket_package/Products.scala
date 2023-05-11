package shoppingBasket_package

object Products {
  val SOUP = "soup"
  val BREAD = "bread"
  val MILK = "milk"
  val APPLES = "apples"
}

sealed trait Item {
  val price: Int
}
case class Soup(price: Int = 65) extends Item
case class Bread(price: Int = 80) extends Item
case class Milk(price: Int = 130) extends Item
case class Apple(price: Int = 100) extends Item
