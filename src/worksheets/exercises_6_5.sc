import scala.util.{Failure, Success, Try}

object Exercises {

  def addOptions(op1: Option[Int], op2: Option[Int]): Option[Int] = for {
    a <- op1
    b <- op2
  } yield a + b

  def addOptionsMap(op1: Option[Int], op2: Option[Int]): Option[Int] =
    op1 flatMap { a => op2 map { b => a + b } }

  addOptions(Option(1), Option(1))
  addOptions(None, Option(1))
  addOptionsMap(Option(1), Option(1))
  addOptionsMap(None, Option(1))

  def addOptions(op1: Option[Int], op2: Option[Int], op3: Option[Int]): Option[Int] = for {
    a <- op1
    b <- op2
    c <- op3
  } yield a + b + c

  def addOptionsMap(op1: Option[Int], op2: Option[Int], op3: Option[Int]): Option[Int] =
    op1 flatMap { a => op2 flatMap { b => op3 map { c => a + b + c } } }

  addOptions(Option(1), Option(1), Option(1))
  addOptions(None, Option(1), Option(1))
  addOptionsMap(Option(1), Option(1), Option(1))
  addOptionsMap(None, Option(1), Option(1))

  def divide(num: Int, den: Int): Option[Int] = if (den == 0) None else Option(num / den)

  def divideOptions(num: Option[Int], den: Option[Int]): Option[Int] = for {
    a <- num
    b <- den
    c <- divide(a, b)
  } yield c

  def divideOptionsMap(num: Option[Int], den: Option[Int]): Option[Int] =
    num flatMap { a => den flatMap { b => divide(a, b) } }

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    val result = for {
      a <- Try(operand1.toInt)
      b <- Try(operand2.toInt)
      ans <- operator match {
        case "+" => Try(a + b)
        case "-" => Try(a - b)
        case "*" => Try(a * b)
        case "/" => Try(a / b)
        case _ => Failure(new UnsupportedOperationException())
      }
    } yield ans

    result match {
      case Success(number) => println(s"The answer is $number!")
      case Failure(exception) => println(s"Error calculating $exception")
    }
  }

  calculator("1", "+", "1")
  calculator("2", "-", "1")
  calculator("10", "*", "3")
  calculator("9", "/", "3")
  calculator("a", "+", "1")
  calculator("1", "!", "1")

}