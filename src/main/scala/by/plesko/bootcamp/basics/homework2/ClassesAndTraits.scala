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

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Movable2D
  }

  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
    def area: Option[Double]
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def area: Option[Double] = None
    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def area: Option[Double] = Option(Math.PI * Math.pow(radius, 2))
    override def move(dx: Double, dy: Double): Shape2D = Circle(x + dx, y + dy, radius)
  }

  // Square is a special case of rectangle
  final case class Rectangle(minX: Double, minY: Double, maxX: Double, maxY: Double) extends Shape2D {
    override def x: Double = (maxX + minX) / 2
    override def y: Double = (maxY + minY) / 2
    override def area: Option[Double] = Option((maxX - minX) * (maxY - minY))
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(minX + dx, minY + dy, minX + dx, maxY + dy)
  }

  final case class Triangle(pointA: Point2D, pointB: Point2D, pointC: Point2D) extends Shape2D {
    override def x: Double = (pointA.x + pointB.x + pointC.x) / 3
    override def y: Double = (pointA.y + pointB.y + pointC.y) / 3
    override def minX: Double = List(pointA.x, pointB.x, pointC.x).min
    override def maxX: Double = List(pointA.x, pointB.x, pointC.x).max
    override def minY: Double = List(pointA.y, pointB.y, pointC.y).min
    override def maxY: Double = List(pointA.y, pointB.y, pointC.y).max
    override def area: Option[Double] = ???
    override def move(dx: Double, dy: Double): Triangle =
      Triangle(Point2D(pointA.x + dx, pointA.y + dy), Point2D(pointB.x + dx, pointB.y + dy), Point2D(pointB.x + dx, pointB.y + dy))
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Movable3D
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Option[Double]
    def volume: Option[Double]
  }

  final case class Origin() extends Shape3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def surfaceArea: Option[Double] = None
    override def volume: Option[Double] = None
    override def move(dx: Double, dy: Double, dz: Double): Origin = ???
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def surfaceArea: Option[Double] = None
    override def volume: Option[Double] = None
    override def move(dx: Double, dy: Double, dz: Double): Origin = ???
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius
    override def surfaceArea: Option[Double] = Option(4 * Math.PI * Math.pow(radius, 2))
    override def volume: Option[Double] = Option((4 * Math.PI * Math.pow(radius, 3)) / 3)
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
  }

  // Cube is a special case of cuboid
  final case class Cuboid(minX: Double, minY: Double, minZ: Double, maxX: Double, maxY: Double, maxZ: Double) extends Shape3D {
    override def x: Double = (maxX + minX) / 2
    override def y: Double = (maxY + minY) / 2
    override def z: Double = (maxZ + minZ) / 2
    override def surfaceArea: Option[Double] = Option(2 * ((maxZ - minZ) * (maxX - minX) + (maxX - minX) * (maxY - minY) + (maxZ - minZ) * (maxY - minY)))
    override def volume: Option[Double] = Option((maxX - minX) * (maxY - minY) * (maxZ - minZ))
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(minX + dx, minY + dy, minZ + dz, maxX + dx, maxY + dy, maxZ + dz)
  }

  final case class Tetrahedron(pointA: Point3D, pointB: Point3D, pointC: Point3D) extends Shape3D {
    override def x: Double = (pointA.x + pointB.x + pointC.x) / 3
    override def y: Double = (pointA.y + pointB.y + pointC.y) / 3
    override def z: Double = (pointA.z + pointB.z + pointC.z) / 3
    override def minX: Double = List(pointA.x, pointB.x, pointC.x).min
    override def maxX: Double = List(pointA.x, pointB.x, pointC.x).max
    override def minY: Double = List(pointA.y, pointB.y, pointC.y).min
    override def maxY: Double = List(pointA.y, pointB.y, pointC.y).max
    override def minZ: Double = List(pointA.z, pointB.z, pointC.z).min
    override def maxZ: Double = List(pointA.z, pointB.z, pointC.z).max
    override def surfaceArea: Option[Double] = ???
    override def volume: Option[Double] = ???
    override def move(dx: Double, dy: Double, dz: Double): Tetrahedron =
      Tetrahedron(Point3D(pointA.x + dx, pointA.y + dy, pointA.z + dz), Point3D(pointB.x + dx, pointB.y + dy, pointB.z + dz), Point3D(pointC.x + dx, pointC.y + dy, pointC.z + dz))
  }

}
