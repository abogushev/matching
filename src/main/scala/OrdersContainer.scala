import Operation._
import scala.collection.mutable

class OrdersContainer {
    private val orders = mutable.HashMap.empty[OrderKey, mutable.ArrayBuffer[Order]]

    def addOrder(order: Order): Unit = {
        orders.keySet.find(_ == order.key) match {
            case Some(key) => orders(key) += order
            case None => orders += order.key -> mutable.ArrayBuffer(order)
        }
    }

    def popAltOrder(order: Order): Option[Order] = {
        for {
            ordersArr <- orders.get(order.key)
            altOp = if (order.operation == Sell) Buy else Sell
            altOrder <- ordersArr.find(o => o.operation == altOp && o.client.name != order.client.name)
        } yield {
            ordersArr -= altOrder
            if (ordersArr.isEmpty)
                orders -= order.key
            altOrder
        }
    }
}
