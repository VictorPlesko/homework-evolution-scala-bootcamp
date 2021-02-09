package by.plesko.bootcamp.basics.homework4

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
    pairPoints.foldLeft(Int.MinValue){
      case (maxArea, (point1, point2)) => maxArea max point2.head - point1.head
    }
  }


}
