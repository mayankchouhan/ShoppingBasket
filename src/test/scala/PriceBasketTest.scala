//package shoppingBasket_package

import shoppingBasket_package._

import org.scalatest.{FlatSpecLike, Matchers}


class PriceBasketTest extends FlatSpecLike with Matchers {
  val priceBasket = new PriceBasket {}
  implicit val offers = List(ApplesOffer, SoupOffer)

  "Validate basket" should "return error message is invalid/unknown product" in {
    priceBasket.validateInput(List("apples", "orange", "soup")) shouldBe Left("invalid input : Incorrect Input Item orange")
  }

  it should "return products" in {
    priceBasket.validateInput(List("apples", "soup", "bread", "milk")) shouldBe Right(List(Apple(), Soup(), Bread(), Milk()))
  }

  "Process Basket" should "return error message is invalid/unknown product" in {
    priceBasket.processBasket(List("apples", "orange", "soup")) shouldBe Left("invalid input : Incorrect Input Item orange")
  }


  "Apple discount" should "return output  when apple discount is applied" in {
    priceBasket.processBasket(List("apples", "soup", "soup")) shouldBe Right(Output(2.30, "Apple on 10% discount : £ 0.10", 2.20))
  }

  it should "return output  when multiple apples are present" in {
    priceBasket.processBasket(List("apples", "apples", "bread", "milk")) shouldBe Right(Output(4.10,
      "Apple on 10% discount : £ 0.20",
      3.90))
  }

  "Soup discount" should "return output, soup discount is applied" in {
    priceBasket.processBasket(List("soup", "soup", "bread")) shouldBe Right(Output(2.10,
      "2 X Soup makes Bread half price : £ 0.40",
      1.70))
  }

  it should "return output, soup discount is applied - multiple soups and breads" in {
    priceBasket.processBasket(List("soup", "soup", "soup", "soup", "bread", "bread", "bread", "bread")) shouldBe Right(Output(5.80,
      "2 X Soup makes Bread half price : £ 1.60",
      4.20))
  }

  it should "return output, soup discount is not applied" in {
    priceBasket.processBasket(List("soup", "bread", "bread")) shouldBe Right(Output(2.25,
      "(No offers available)",
      2.25))
  }

  "Multi discount offer" should "apply all offers" in {
    priceBasket.processBasket(List("apples", "soup", "soup", "bread", "bread"))(List(ApplesOffer, SoupOffer)) shouldBe Right(Output(3.90,
      "Apple on 10% discount : £ 0.10\n2 X Soup makes Bread half price : £ 0.80",
      3.00))
  }

  it should "apply soup offers if only soup offer is set even if there is apple" in {
    priceBasket.processBasket(List("apples", "soup", "soup", "bread", "bread"))(List(SoupOffer)) shouldBe Right(Output(3.90,
      "2 X Soup makes Bread half price : £ 0.80",
      3.10))
  }

  it should "apply apple offers if only apples offer is set even if there is soup and bread" in {
    priceBasket.processBasket(List("apples", "soup", "soup", "bread", "bread"))(List(ApplesOffer)) shouldBe Right(Output(3.90,
      "Apple on 10% discount : £ 0.10",
      3.80))
  }

  "No discount applied" should "return output when no discount is applied" in {
    priceBasket.processBasket(List("soup", "bread", "bread", "bread", "milk", "milk", "milk")) shouldBe Right(Output(6.95,
      "(No offers available)",
      6.95))
  }

}
