package by.plesko.bootcamp.basics.homework4

object Collections {
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.foldLeft(Array.empty[Int])((acc, x) => acc :+ x + acc.lastOption.getOrElse(0))
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.indices.take(n).flatMap(i => Array(nums(i), nums(i + n))).toArray
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.foldLeft(Int.MinValue)((maxWealth, acc) => maxWealth max acc.sum)
  }
}
