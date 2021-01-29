package by.plesko.bootcamp.basics.homework1

import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Option[Int] =
    if (a == 0 || b == 0) None
    else Some(Math.abs(a * b) / gcd(a, b).get)

  @tailrec
  def gcd(a: Int, b: Int): Option[Int] = {
    if (a == 0 && b == 0) None
    else if (b == 0) Some(Math.abs(a))
    else gcd(b, a % b)
  }
}
