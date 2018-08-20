import Operation.{Buy, Sell}
import Stock.{A, B, C, D}
import scala.collection.mutable

class OrdersSpec extends BaseSpec {
    def add(ordersContainer: OrdersContainer, orders: Order*) = {
        orders.foreach(ordersContainer.addOrder)
    }
    "Orders" should "find altOrder" in {
        val ordersContainer = new OrdersContainer
        val c1 = new Client(name = "C1", money = 1000, stockBalances = mutable.LinkedHashMap(A -> 1, B -> 1, C -> 1, D -> 1))
        val c2 = new Client(name = "C2", money = 2000, stockBalances = mutable.LinkedHashMap(A -> 2, B -> 2, C -> 2, D -> 2))

        val buyOrder = new Order(c1, Buy, A, 10, 1)
        val sellOrder = new Order(c2, Sell, A, 10, 1)

        add(ordersContainer,
            sellOrder,
            new Order(c1, Sell, A, 10, 1),
            new Order(c2, Buy, A, 10, 1),
            new Order(c2, Sell, B, 10, 1),
            new Order(c2, Sell, A, 9, 1),
            new Order(c2, Sell, A, 10, 2)
        )

        sellOrder should be theSameInstanceAs ordersContainer.popAltOrder(buyOrder).get
    }

    it should "not find altOrder" in {
        val ordersContainer = new OrdersContainer
        val c1 = new Client(name = "C1", money = 1000, stockBalances = mutable.LinkedHashMap(A -> 1, B -> 1, C -> 1, D -> 1))
        val c2 = new Client(name = "C2", money = 2000, stockBalances = mutable.LinkedHashMap(A -> 2, B -> 2, C -> 2, D -> 2))

        val buyOrder = new Order(c1, Buy, A, 10, 1)
        add(ordersContainer,
            new Order(c1, Sell, A, 10, 1),
            new Order(c2, Buy, A, 10, 1),
            new Order(c2, Sell, B, 10, 1),
            new Order(c2, Sell, A, 9, 1),
            new Order(c2, Sell, A, 10, 2)
        )

        ordersContainer.popAltOrder(buyOrder) shouldBe empty
    }
}
