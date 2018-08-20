import Operation.{Buy, Sell}
import Stock._
import scala.collection.mutable

class BuysSpec extends BaseSpec {
    def checkBalance(client: Client, blns: (Stock, Int)*) = {
        Stock.values.forall { s =>
            val oldVal = client.stockBalances(s)
            val expVal = blns.find(_._1 == s).map(_._2).getOrElse(oldVal)
            oldVal == expVal
        } should be (true)
    }

    "C1" should "buy 1 of A" in {
        val orderManager = new OrderManager
        val c1 = new Client(name = "C1", money = 1000, stockBalances = mutable.LinkedHashMap(A -> 1, B -> 1, C -> 1, D -> 1))
        val c2 = new Client(name = "C2", money = 2000, stockBalances = mutable.LinkedHashMap(A -> 2, B -> 2, C -> 2, D -> 2))
        val orders = List[Order](
            new Order(c1, Buy, A, 10, 1),
            new Order(c2, Sell, A, 10, 1)
        )

        orders.foreach(orderManager.makeDeal)

        c1.money should be (990)
        checkBalance(c1, A -> 2)

        c2.money should be (2010)
        checkBalance(c2, A -> 1)
    }

    it should "buy all of A from C2" in {
        val orderManager = new OrderManager
        val c1 = new Client(name = "C1", money = 1000, stockBalances = mutable.LinkedHashMap(A -> 1, B -> 1, C -> 1, D -> 1))
        val c2 = new Client(name = "C2", money = 2000, stockBalances = mutable.LinkedHashMap(A -> 2, B -> 2, C -> 2, D -> 2))
        val c3 = new Client(name = "C3", money = 3000, stockBalances = mutable.LinkedHashMap(A -> 3, B -> 3, C -> 3, D -> 3))

        val orders = List[Order](
            new Order(c2, Sell, A, 10, 1),
            new Order(c2, Sell, A, 10, 1),
            new Order(c1, Buy, A, 10, 1),
            new Order(c1, Buy, A, 10, 1),
            new Order(c3, Sell, A, 10, 1),
            new Order(c3, Buy, A, 10, 1)
        )

        orders.foreach(orderManager.makeDeal)

        c1.money should be (980)
        checkBalance(c1, A -> 3)

        c2.money should be (2020)
        checkBalance(c2, A -> 0)

        c3.money should be (3000)
        checkBalance(c3)
    }

    it should "not buy A" in {
        val orderManager = new OrderManager
        val c1 = new Client(name = "C1", money = 1000, stockBalances = mutable.LinkedHashMap(A -> 1, B -> 1, C -> 1, D -> 1))
        val c2 = new Client(name = "C2", money = 2000, stockBalances = mutable.LinkedHashMap(A -> 2, B -> 2, C -> 2, D -> 2))

        val orders = List[Order](
            new Order(c1, Buy, A, 10, 1),
            new Order(c2, Buy, A, 10, 1),
            new Order(c2, Sell, B, 10, 1),
            new Order(c2, Sell, A, 9, 1),
            new Order(c2, Sell, A, 10, 2)
        )

        orders.foreach(orderManager.makeDeal)

        c1.money should be (1000)
        checkBalance(c1)

        c2.money should be (2000)
        checkBalance(c2)
    }
}
