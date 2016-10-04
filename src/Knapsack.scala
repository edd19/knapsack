/**
  * Created by ndizera on 27/09/2016.
  */
class Knapsack(capacity: Int, items: List[(Int, Int, Int)]) {
  val items_sorted = items.sortWith(sortByRatio)
  val cache = collection.mutable.Map.empty[(Int, Int), (Int, List[Int])]

  def sortByRatio(item1: (Int, Int, Int), item2: (Int, Int, Int)) = {
    item1._1 / item1._2 > item2._1/item2._2
  }


  def DP_solver(): (Int, Array[Int]) = {
    val upperBound = items_sorted.reduce((x, y) => (x._1+y._1, 0, 0))._1
    val (max_value, items_taken) = DP(capacity, items_sorted, upperBound, 0, 0, Nil)
    (max_value, format_solution(items_taken))
  }


  def format_solution(items_taken: List[Int]): Array[Int] = {
    val items_value = new Array[Int](items.length)
    for(id <- items_taken){
      items_value(id-1) = 1
    }
    items_value
  }


  def DP(capacity: Int, items: List[(Int, Int, Int)], upperBound: Int, max_value: Int, value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    items match {
      case Nil => (value, items_taken)
      case (v, w, id) :: items_left => {
        if (upperBound < max_value ) (value, items_taken)
        else if (w > capacity) DP_(capacity, items_left, upperBound-v, max_value, value, items_taken)
        else {
          val (value_1, items_taken_1) = DP_(capacity-w, items_left, upperBound, max_value, value+v, id :: items_taken)
          val (value_2, items_taken_2) = {
            if (value_1 > max_value) DP_(capacity, items_left, upperBound-v, value_1, value, items_taken)
            else DP_(capacity, items_left, upperBound-v, max_value, value, items_taken)
          }
          if (value_1 > value_2) (value_1, items_taken_1)
          else (value_2, items_taken_2)
        }
      }
    }
  }

  def DP_(capacity: Int, items: List[(Int, Int, Int)], upperBound: Int, max_value: Int, value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    cache.getOrElseUpdate((capacity, items.length), DP(capacity, items, upperBound, max_value, value, items_taken))
  }

}
