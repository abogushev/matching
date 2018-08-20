import Operation._

class OrderManager {
    private val ordersContainer = new OrdersContainer

    def makeDeal(order: Order): Unit = {
        val (buyer, seller) = if (order.operation == Sell) {
            (ordersContainer.popAltOrder(order).map(_.client), Some(order.client))
        }
        else {
            (Some(order.client), ordersContainer.popAltOrder(order).map(_.client))
        }
        (buyer, seller) match {
            case (Some(b), Some(s)) =>
                b.money -= order.price
                s.money += order.price
                b.stockBalances(order.stock) += order.count
                s.stockBalances(order.stock) -= order.count

            case _ => ordersContainer.addOrder(order)
        }
    }
}