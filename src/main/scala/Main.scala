import scala.io.Source
import java.io.File
import java.io.PrintWriter

object Main extends App {
    val clientsStrs = IO.read("/clients.txt")
    val ordersStrs = IO.read("/orders.txt")

    val orderManager = new OrderManager
    val clientsMap = clientsStrs.flatMap(Client.apply).map(c => c.name -> c).toMap

    ordersStrs.flatMap(Order(_, clientsMap)).foreach(orderManager.makeDeal)

    IO.write(clientsMap.values.toList)
}

object IO {
    def read(fileName: String) = Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines()

    def write(clients: List[Client]) = {
        val writer = new PrintWriter(new File("src/main/resources/result.txt"))
        writer.write(clients.sortBy(_.name).mkString("\n"))
        writer.close()
    }
}
