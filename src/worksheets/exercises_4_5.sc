object Exercises {

  sealed trait TrafficLight {
    def next(current: TrafficLight): TrafficLight = current match {
      case Red => Green
      case Orange => Red
      case Green => Orange
    }
  }

  case object Red extends TrafficLight

  case object Orange extends TrafficLight

  case object Green extends TrafficLight


  sealed trait Calculation

  case class Success(result: Int) extends Calculation

  case class Failure(reason: String) extends Calculation

  object Calculator {
    def +(calculation: Calculation, operand: Int): Calculation = calculation match {
      case Failure(reason) => Failure(reason)
      case Success(result) => Success(result + operand)
    }

    def -(calculation: Calculation, operand: Int): Calculation = calculation match {
      case Failure(reason) => Failure(reason)
      case Success(result) => Success(result - operand)
    }

    def /(calculation: Calculation, operand: Int): Calculation = calculation match {
      case Success(result) => operand match {
        case 0 => Failure("Division by zero")
        case _ => Success(result / operand)
      }
      case Failure(reason) => Failure(reason)
    }
  }

  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

}

