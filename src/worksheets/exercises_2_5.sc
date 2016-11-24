object Exercises {

  object calc {
    def square(x: Double): Double = x * x
    def cube(x: Double): Double = x * square(x)
  }

  assert(calc.square(2.0) == 4.0)
  assert(calc.square(3.0) == 9.0)
  assert(calc.square(-2.0) == 4.0)

}



