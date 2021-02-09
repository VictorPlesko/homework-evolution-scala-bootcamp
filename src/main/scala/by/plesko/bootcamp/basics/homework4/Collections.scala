package by.plesko.bootcamp.basics.homework4

object Collections {
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.foldLeft(Array.empty[Int])((acc, x) => acc :+ x + acc.lastOption.getOrElse(0))
  }


}
