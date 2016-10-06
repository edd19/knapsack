/**
  * Created by ndizera on 27/09/2016.
  */
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val instanceFile = args(0)
    val (capacity, items) = parseFile(instanceFile)
    val knapsack = new Knapsack(capacity, items)
    val (max_value, items_value) = knapsack.Linear_random_solver()
    println(max_value)
    for (item_value <- items_value){
      print(item_value)
      print(" ")
    }
  }

  /**
    *
    * @param filepath path to the file
    * @return a tuple of total capacity of the knapsack and list of items
    */
  def parseFile(filepath: String): (Int, List[(Int, Int, Int)]) ={
    val source = Source.fromFile(filepath)
    val lines = source.getLines().toArray
    val header = lines(0).split("\\s+")
    val capacity = header(1).toInt
    var l = List[(Int, Int, Int)]()
    for(i <- 1 until lines.length){
      val tokens = lines(i).split("\\s+")
      val item = (tokens(0).toInt, tokens(1).toInt, i)
      l = l ::: List(item)
    }
    return (capacity, l)
  }
}