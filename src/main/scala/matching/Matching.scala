package matching

import scala.io.Source

object Matching {
  sealed abstract class Order {
    val price: Int
    val count: Int
    val client: String
    val stock: String

    val key: String = s"$price $count $stock"
  }

  case class ByeOrder(price: Int, count: Int, client: String, stock: String) extends Order
  case class SellOrder(price: Int, count: Int, client: String, stock: String) extends Order

  case class Client(name: String, balans: Int, stocks: Map[String, Int])

  def matches(bye: Seq[ByeOrder], sell: Seq[SellOrder], pairs: Seq[(ByeOrder, SellOrder)] = Seq()): Seq[(ByeOrder, SellOrder)] = {
    (bye, sell) match {
      case (Seq(),_) => pairs
      case (_,Seq()) => pairs
      case ((b +: bs), (s +: ss)) => {
        val r = b.key compare s.key
        if (r == 0) {
          if(b.client == s.client) { 
            matches(bs, ss, pairs)
          } else {
            matches(bs, ss, (b,s) +: pairs)
          }
        } else if (r < 0) {
          matches(bs, sell, pairs)
        } else {
          matches(bye, ss, pairs)
        }
      }
    }
  }

  def process(clients: Map[String, Client], transaction: (ByeOrder, SellOrder)): Map[String, Client] = {
    val (ByeOrder(price, count, byer, stock), SellOrder(_,_, seller,_)) = transaction // Assume _1.attr == _2.attr for attr in stock, price, count
    val sum = price * count
    val b = clients(byer)
    val s = clients(seller)
    def updateStocks(s: Map[String, Int], u: Int => Int): Map[String, Int] = {
      s + (stock -> u(s(stock))) 
    }

    clients ++ Seq(byer ->   b.copy(balans = b.balans - sum, stocks = updateStocks(b.stocks, _ + count)),
                    seller -> s.copy(balans = s.balans + sum, stocks = updateStocks(s.stocks, _ - count)))
  }

  val stockNames = Seq("A", "B", "C", "D")

  def mkClient(c: String): (String, Client) = {
    val name  +: balans +: stocks = c.split("\t").toSeq
    name -> Client(name, balans.toInt, (stockNames zip stocks.map(_.toInt)).toMap)
  }

  def readOrder(clients: Set[String])(o: String, orders: (Seq[ByeOrder], Seq[SellOrder]) = (Seq(), Seq())): (Seq[ByeOrder], Seq[SellOrder]) = {
    o.split("\t").toSeq match {
      case Seq(cl, "b", stock, price, count) if(clients(cl)) =>
        ((ByeOrder(price.toInt, count.toInt, cl, stock) +: orders._1), orders._2)
      case Seq(cl, "s", stock, price, count) if(clients(cl)) =>
        (orders._1, (SellOrder(price.toInt, count.toInt, cl, stock) +: orders._2))
    }
  }

  def main(args: Array[String]): Unit = {
    val clients = Source.fromFile("clients.txt").getLines.map(mkClient).toMap
    val (bye, sell) = Source.fromFile("orders.txt").getLines.foldRight((Seq[ByeOrder](), Seq[SellOrder]()))(readOrder(clients.keySet))
    val result = matches(bye.sortBy(_.key), sell.sortBy(_.key)).foldLeft(clients)(process).values
    new java.io.PrintWriter("result.txt") {
      result foreach { case Client(name, balans, stocks) => write(s"$name\t$balans\t${(stockNames map stocks).mkString("\t")}\n") }
      close
    }
  }
}
