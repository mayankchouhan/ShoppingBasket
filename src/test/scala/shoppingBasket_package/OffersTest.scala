package shoppingBasket_package

import org.scalatest.{Matchers, FlatSpecLike}

class OffersTest extends FlatSpecLike with Matchers {

  "Apple offers" should "get offers on multiple apples" in {
    ApplesOffer(Map(Apple() -> 4)) shouldBe Some(OfferDetails(List(Apple()), "Apple on 10% discount", 400, 360))
  }

  "Soup offers" should "No offers on 1 soup" in {
    SoupOffer(Map(Soup() -> 1, Bread() -> 3)) shouldBe None
  }

  it should "give offers for multiple pair of soups and breads" in {
    SoupOffer(Map(Soup() -> 4, Bread() -> 6)) shouldBe Some(OfferDetails(List(Soup(), Bread()), "2 X Soup makes Bread half price", 740, 500))
  }

  "Products without offer" should "get no discount" in {
    ItemsWithoutOffer.processItemsWithoutOffer(Map(Soup() -> 1, Bread() -> 1, Milk() -> 1), List()) shouldBe Some(OfferDetails(List(), "", 275, 275))
  }

  it should "not offer discount while buying bread on its own" in {
    ItemsWithoutOffer.processItemsWithoutOffer(Map(Soup() -> 2, Bread() -> 1, Milk() -> 1), List(Soup(), Bread())) shouldBe Some(OfferDetails(List(), "", 130, 130))
  }

}
