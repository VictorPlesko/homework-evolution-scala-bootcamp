package by.plesko.bootcamp.basics.homework4

import scala.annotation.tailrec

object Collections {

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.foldLeft(Array.empty[Int])((acc, x) => acc :+ x + acc.lastOption.getOrElse(0))
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.indices.take(n).flatMap(i => Array(nums(i), nums(i + n))).toArray
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.foldLeft(Int.MinValue)((maxWealth, acc) => maxWealth max acc.sum)
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.foldLeft(Array.empty[Boolean])((res, candy) =>
      if (candies.forall(_ <= candy + extraCandies)) res :+ true else res :+ false)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedPoints = points.sortBy(_.head)
    val pairPoints = sortedPoints zip sortedPoints.tail
    pairPoints.foldLeft(Int.MinValue) {
      case (maxArea, (point1, point2)) => maxArea max point2.head - point1.head
    }
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    s.foldLeft(0, 0) {
      case ((maxDepth, acc), symbol) =>
        if (symbol == '(') (maxDepth max acc + 1, acc + 1)
        else if (symbol == ')') (maxDepth, acc - 1)
        else (maxDepth, acc)
    }._1
  }

  // Tasks from the lecture

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {

    @tailrec
    def go(listResidue: List[T], listAcc: List[T]): List[T] = {
      if (listResidue == Nil) listAcc
      else go(listResidue.tail, listAcc :+ f(listAcc.last, listResidue.head))
    }

    go(list, List(zero))
  }

  def count(s: String): List[(Char, Int)] = {
    s.foldLeft(List.empty[(Char, Int)]) {
      case (Nil, symbol) => Nil :+ (symbol, 1)
      case (acc, symbol) =>
        if (acc.last._1 == symbol) acc.updated(acc.length - 1, (symbol, acc.last._2 + 1)) else acc :+ (symbol, 1)
    }
  }

  def min(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case _   => Some(list.foldLeft(Int.MaxValue)((acc, x) => x min acc))
    }

    //list.reduceOption((a,b) => a min b)

    /* @tailrec
     def go(listRec: List[Int], min: Int): Int = {
       if (listRec == Nil) min
       else if (min < listRec.head) go(listRec.tail, min)
       else go(listRec.tail, listRec.head)
     }

     list match {
       case Nil => None
       case _   => Some(go(list, Int.MaxValue))
     }*/
  }


  // Tasks from the "DataStructures" file

  // Exercise. Write a function that checks if all values in a `List` are equal.
  // Think about what you think your function should return if `list` is empty, and why.
  def allEqual[T](list: List[T]): Boolean = {
    list match {
      case x if x.isEmpty => true
      case x              => x.forall(_ == list.head)
    }
  }

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  // Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
  // `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
  // in `vegetablePrices`.
  val totalVegetableCost: Int = {
    vegetableAmounts.foldLeft(0)((acc, x) => acc + x._2 * vegetablePrices.getOrElse(x._1, 10))
  }

  // Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
  // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
  //
  // For example, the total weight of "olives" is 2 * 32 == 64.
  val totalVegetableWeights: Map[String, Int] = { // implement here
    for {
      x <- vegetableWeights
      if vegetableAmounts.contains(x._1)
    } yield (x._1 -> x._2 * vegetableAmounts(x._1))
  }

  // Exercise: Return a set with all subsets of the provided set `set` with `n` elements
  // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
  // Hints for implementation:
  //   - Handle the trivial case where `n == 1`.
  //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
  //     that don't include `elem`, and add `elem` to them.
  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
    set.subsets(n).toSet
  }

  // Homework
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] =
    map.groupBy(_._2).flatMap(x => List((x._2.keySet, x._1))).toList.sortBy(_._2)
}
