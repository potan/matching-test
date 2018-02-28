package test.matches

import org.junit.runner._
import org.specs2.runner._
import org.specs2.mutable._

import matching.Matching._

@RunWith(classOf[JUnitRunner])
class MathesSpec extends Specification {
  "Matches" should {
    "Match orders with same stock, price and count" in {
      matches(
        Seq(ByeOrder(1, 1, "C1", "A"), ByeOrder(1, 3, "C2", "A"), ByeOrder(2, 1, "C3", "A")),
        Seq(SellOrder(1, 2, "C4", "A"), SellOrder(1, 3, "C5", "A"), SellOrder(2, 4, "C6", "A"))
      ) must_=== Seq((ByeOrder(1, 3, "C2", "A"), SellOrder(1, 3, "C5", "A")))
    }
    "Don't match orders same clients" in {
      matches(
        Seq(ByeOrder(1, 1, "C1", "A")),
        Seq(SellOrder(1, 1, "C1", "A"))
      ) must_=== Seq()
    }
    "Update clients state" in {
      process(Map("C1" -> Client("C1", 5, Map("A" -> 2)), "C2" -> Client("C2", 4, Map("A" -> 3))),
              (ByeOrder(2, 1, "C1", "A"), SellOrder(2, 1, "C2", "A"))
             ) must_=== Map("C1" -> Client("C1", 3, Map("A" -> 3)), "C2" -> Client("C2", 6, Map("A" -> 2)))
    }
  }
}
