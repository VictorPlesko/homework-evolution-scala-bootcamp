package by.plesko.bootcamp.basics.homework2

object ClassesAndTraits {

  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Movable
  }

  sealed trait Shape2D extends Located2D with Bounded2D with Movable {
    def area: Double
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    override def area: Double = Math.PI * Math.pow(radius, 2)
    override def move(dx: Double, dy: Double): Shape2D = Circle(x + dx, y + dy, radius)
  }

  // Square is a special case of rectangle
  final case class Rectangle(minX: Double, minY: Double, maxX: Double, maxY: Double) extends Shape2D {
    override def x: Double = (maxX - minX) / 2 + minX
    override def y: Double = (maxY - minY) / 2 + minY

    override def area: Double = (maxX - minX) * (maxY - minY)
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(minX + dx, minY + dy, minX + dx, maxY + dy)
  }

  final case class Triangle(xA: Double, yA: Double, xB: Double, yB: Double, xC: Double, yC: Double) extends Shape2D {
    override def x: Double = (xA + xB + xC) / 3
    override def y: Double = (yA + yB + yC) / 3
    override def minX: Double = List(xA, xB, xC).min
    override def maxX: Double = List(xA, xB, xC).max
    override def minY: Double = List(yA, yB, yC).min
    override def maxY: Double = List(yA, yB, yC).max

    override def area: Double = ???
    override def move(dx: Double, dy: Double): Triangle = Triangle(xA + dx, yA + dy, xB + dx, yB + dy, xC + dx, yC + dy)
  }

}
