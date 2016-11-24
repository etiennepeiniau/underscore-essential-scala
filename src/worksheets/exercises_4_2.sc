object Exercises {


  sealed trait Shape {
    def sides: Int

    def perimeter: Double

    def area: Double

    def color: Color
  }

  final case class Circle(radius: Double, color: Color) extends Shape {
    def sides: Int = 1

    def perimeter: Double = 2 * Math.PI * radius

    def area: Double = Math.PI * radius * radius
  }

  sealed trait Rectangular extends Shape {
    def sides: Int = 4

    def width: Double

    def height: Double

    def perimeter: Double = 2 * width + 2 * height

    def area: Double = width * height
  }

  final case class Rectangle(width: Double, height: Double, color: Color) extends Rectangular

  final case class Square(size: Double, color: Color) extends Rectangular {
    val width: Double = size
    val height: Double = size
  }

  object Draw {

    def apply(shape: Shape): String = shape match {
      case Circle(radius, color) => s"A ${Draw(color)} circle of radius ${radius}cm"
      case Square(size, color) => s"A ${Draw(color)} square of size ${size}cm"
      case Rectangle(width, height, color) => s"A ${Draw(color)} rectangle of width ${width}cm and height ${height}cm"
    }

    def apply(color: Color): String = color match {
      case Red => "red"
      case Yellow => "yellow"
      case Pink => "pink"
      case matched => if (matched.isLight) "light" else "dark"
    }

  }

  sealed trait Color {
    def red: Double

    def green: Double

    def blue: Double

    def isLight = (red + green + blue) / 3.0 > 0.5

    def isDark = !isLight
  }

  object Red extends Color {
    val red = 1.0
    val green = 0.0
    val blue = 0.0
  }

  object Yellow extends Color {
    val red = 1.0
    val green = 1.0
    val blue = 0.0
  }

  object Pink extends Color {
    val red = 1.0
    val green = 0.0
    val blue = 1.0
  }

  final case class CustomColor(red: Double, green: Double, blue: Double) extends Color

  Draw(Circle(10, Pink))
  Draw(Rectangle(3, 4, CustomColor(0.4, 0.4, 0.6)))

  object divide {
    def apply(x: Int, y: Int): DivisionResult = y match {
      case 0 => Infinite
      case _ => Finite(x / y)
    }
  }

  sealed trait DivisionResult

  case object Infinite extends DivisionResult

  final case class Finite(r: Int) extends DivisionResult

  divide(1, 2)
  divide(1, 0)

}