import Stock.Stock
import scala.collection.mutable

class Client(val name: String, var money: Int, val stockBalances: mutable.LinkedHashMap[Stock, Int]) {
    override def toString: String = s"""$name\t$money\t${stockBalances.values.mkString("\t")}"""
}

object Client {
    private val rgx = """(.*)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)""".r
    import Stock._

    def apply(str: String): Option[Client] = str match {
        case rgx(name, money, a, b, c, d) =>
            val stockBalances = mutable.LinkedHashMap(A -> a.toInt, B -> b.toInt, C -> c.toInt, D -> d.toInt)
            Some(new Client(name, money.toInt, stockBalances))

        case _ => None
    }
}