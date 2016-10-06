/**
  * Created by ndizera on 27/09/2016.
  */
class Knapsack(capacity: Int, items: List[(Int, Int, Int)]) {
  val items_sorted = items.sortWith(sortByRatio)
  val smallest_item = items_sorted.min(Ordering.by((i: (Int, Int, Int)) => i._2))
  val cache = collection.mutable.Map.empty[(Int, Int), (Int, List[Int])]


  def sortByRatio(item1: (Int, Int, Int), item2: (Int, Int, Int)) = {
    if (item1._2 == 0) true
    else if (item2._2 == 0) false
    else if (item1._1.toDouble / item1._2 == item2._1.toDouble / item2._2) item1._1 > item2._1
    else item1._1.toDouble / item1._2 > item2._1.toDouble / item2._2
  }


  def format_solution(items_taken: List[Int]): Array[Int] = {
    val items_value = new Array[Int](items.length)
    for(id <- items_taken){
      items_value(id-1) = 1
    }
    items_value
  }

  def Greedy_solver(): (Int, Array[Int]) = {
    val (max_value, items_taken) = Greedy(capacity, items_sorted, 0, Nil)
    (max_value, format_solution(items_taken))
  }

  def Greedy(capacity: Int, items: List[(Int, Int, Int)], value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    items match {
      case Nil => (value, items_taken)
      case (v, w, id) :: items_left => {
        if (w > capacity) Greedy(capacity, items_left, value, items_taken)
        else {
          Greedy(capacity-w, items_left, value+v, id :: items_taken)
        }
      }
    }
  }

  def Brute_solver(): (Int, Array[Int]) = {
    val (max_value, items_taken) = Brute(capacity, items_sorted, 0, Nil)
    (max_value, format_solution(items_taken))
  }


  def Brute(capacity: Int, items: List[(Int, Int, Int)], value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    items match {
      case Nil => (value, items_taken)
      case (v, w , id) :: items_left => {
        if (w > capacity) Brute_(capacity, items_left, value, items_taken)
        else {
          val (value_1, items_taken_1) = Brute_(capacity-w, items_left, value+v, id :: items_taken)
          val (value_2, items_taken_2) = Brute_(capacity, items_left, value, items_taken)
          if (value_1 > value_2) (value_1, items_taken_1)
          else (value_2, items_taken_2)
        }
      }
    }
  }

  def Brute_(capacity: Int, items: List[(Int, Int, Int)], value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    cache.getOrElseUpdate((capacity, items.size), Brute(capacity, items, value, items_taken))
  }

  def Capa_solver(): (Int, Array[Int]) = {
    val upperBound = items_sorted.reduce((x, y) => (x._1+y._1, 0, 0))._1
    val (max_value, items_taken) = DP_capa(capacity, items_sorted, upperBound, 0, 0, Nil)
    (max_value, format_solution(items_taken))
  }

  def DP_capa(capacity: Int, items: List[(Int, Int, Int)], upperBound: Int, max_value: Int, value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    items match {
      case Nil => (value, items_taken)
      case (v, w, id) :: items_left => {
        if (upperBound < max_value) (value, items_taken)
        else if (w > capacity) DP_capa(capacity, items_left, upperBound-v, max_value, value, items_taken)
        else {
          val (value_1, items_taken_1) = DP_capa(capacity-w, items_left, upperBound, max_value, value+v, id :: items_taken)
          val (value_2, items_taken_2) = {
            if (value_1 > upperBound-v) (value_1, items_taken_1)
            else if (value_1 > max_value) DP_capa(capacity, items_left, upperBound-v, value_1, value, items_taken)
            else DP_capa(capacity, items_left, upperBound-v, max_value, value, items_taken)
          }
          if (value_1 > value_2) (value_1, items_taken_1)
          else (value_2, items_taken_2)
        }
      }
    }
  }


  def Linear_solver(): (Int, Array[Int]) = {
    val upperBound: Double = Linear_upperBound(capacity, items_sorted, 0)
    val (max_value, items_taken) = DP_linear(capacity, items_sorted, upperBound, 0, 0, Nil)
    (max_value, format_solution(items_taken))
  }

  def Linear_random_solver(): (Int, Array[Int]) = {
    val upperBound: Double = Linear_upperBound(capacity, items_sorted, 0)
    var solution: (Int, List[Int]) = (0, Nil)
    for(i <- 0 until 10){
      val temp = remove_random_elements(items_sorted, 25)
      val (v, items_taken) = DP_linear(capacity, temp, upperBound, 0, 0, Nil)
      if (v > solution._1) solution = (v, items_taken)
    }

    (solution._1, format_solution(solution._2))
  }

  def remove_random_elements(l: List[(Int, Int , Int)], n: Int): List[(Int, Int , Int)] = {
    if (n <= 0) l
    else{
      l match{
        case Nil => Nil
        case x :: xs => {
          val isToRemove = scala.util.Random.nextInt() % 2 == 0
          if (isToRemove) remove_random_elements(xs, n-1)
          else x :: remove_random_elements(xs, n)
        }
      }
    }

  }

  def DP_linear(capacity: Int, items: List[(Int, Int, Int)], upperBound: Double, max_value: Int, value: Int, items_taken: List[Int]): (Int, List[Int]) = {
    items match {
      case Nil => (value, items_taken)
      case (v, w, id) :: items_left => {
        if (upperBound < max_value) (value, items_taken)
        else if (smallest_item._2 > capacity) (value, items_taken)
        else if (w > capacity) DP_linear(capacity, items_left,upperBound, max_value, value, items_taken)
        else {
          val (value_1, items_taken_1) = DP_linear(capacity-w, items_left, upperBound, max_value, value+v, id :: items_taken)
          val (value_2, items_taken_2) = {
            if (value_1 > upperBound) (value_1, items_taken_1)
            else if (value_1 > max_value) DP_linear(capacity, items_left, Linear_upperBound(capacity, items_left, value), value_1, value, items_taken)
            else DP_linear(capacity, items_left, Linear_upperBound(capacity, items_left, value), max_value, value, items_taken)
          }
          if (value_1 > value_2) (value_1, items_taken_1)
          else (value_2, items_taken_2)
        }
      }
    }
  }

  def Linear_upperBound(capacity:Int, items: List[(Int, Int, Int)], value: Double): Double = {
    items match{
      case Nil => value
      case (v, w, _) :: items_left => {
        if (w > capacity) value + ((capacity.toDouble / w) * v)
        else Linear_upperBound(capacity-w, items_left, value+v)
      }
    }
  }

}
