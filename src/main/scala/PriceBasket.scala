import shoppingBasket_package._
import ItemsWithoutOffer._



trait PriceBasket {
  type Error = String

  def validateInput(input: List[String]): Either[Error, List[Item]] = {
    input
      .map(CreateObject_DP_Factory.getItem).partition(_.isLeft) match {
      case (Nil, items) => Right(for (Right(i) <- items) yield i)
      case (error, _) =>
        Left((for (Left(s) <- error) yield s)
          .mkString("invalid input : ", " ,", ""))
    }
  }

  def processItems(input: Map[Item, Int])(implicit offers: List[Offers]): Output = {
    val itemsWithOffers: List[Option[OfferDetails]] = offers.map(_.apply(input))
    val itemsWithoutOffers = processItemsWithoutOffer(input, itemsWithOffers.flatten.flatMap(_.offerItems))
    val allItems = (itemsWithoutOffers :: itemsWithOffers).flatten

    val subTotal: BigDecimal = (allItems.map(_.subTotal).sum)/100
    val discountText: String = itemsWithOffers.flatten.map(x => f"${x.offerTitle} : £${(x.subTotal - x.total)/100}%5.2f").mkString("\n")
    val totalPrice: BigDecimal = (allItems.map(_.total).sum)/100
    if (discountText.trim.nonEmpty)
        Output(subTotal, discountText, totalPrice)
    else
        Output(subTotal, "(No offers available)", totalPrice)
  }


  def processBasket(input: List[String])(implicit offers: List[Offers]): Either[Error, Output] = for {
    items <- validateInput(input)
  } yield {
    processItems(items.groupBy(identity).mapValues(_.size))
  }

}

object Application extends PriceBasket {

  /**
    * Main function. This is the start point of the program.
    *
    * input with list of products is input to this program. example - sbt "run apples milk soup soup bread"
    * @param args
    */

  def main(args: Array[String]): Unit = {
    implicit val offers: List[Offers] = List(ApplesOffer, SoupOffer)
    processBasket(args.toList).fold(println, println)
  }
}
