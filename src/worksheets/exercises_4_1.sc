object Exercises {

  trait Feline {
    def color: String
    def sound : String
  }

  trait BigCat extends Feline {
    val sound = "roar"
  }

  case class Cat(color: String, food: String) extends Feline {
    val sound: String = "meow"
  }
  case class Panther(color: String) extends BigCat
  case class Tiger(color: String) extends BigCat
  case class Lion(color: String, maneSize: Int) extends BigCat

  trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  case class Circle(radius: Double) extends Shape {
    def sides: Int = 1
    def perimeter: Double = 2 * Math.PI * radius
    def area: Double = Math.PI * radius * radius
  }

  trait Rectangular extends Shape {
    def sides: Int = 4
    def width: Double
    def height : Double
    def perimeter: Double = 2 * width + 2 * height
    def area: Double = width * height
  }

  case class Rectangle(width: Double, height: Double) extends Rectangular

  case class Square(size: Double) extends Rectangular {
    val width: Double = size
    val height: Double = size
  }

}