import Operation.Operation
import Stock.Stock

class Order(val client: Client, val operation: Operation, val stock: Stock, val pricePerItem: Int, val count: Int) {
    val price: Int = pricePerItem * count
    val key: OrderKey = OrderKey(stock, pricePerItem, count)
}

case class OrderKey(stock: Stock, price: Int, count: Int)

object Order {
    private val rgx = """(.*)\t([bs])\t([ABCD])\t(\d+)\t(\d+)""".r

    def apply(str: String, clients: Map[String, Client]): Option[Order] = str match {
        case rgx(name, op, stock, price, count) =>
            Some(new Order(clients(name), Operation.withName(op), Stock.withName(stock), price.toInt, count.toInt))

        case _ => None
    }
}