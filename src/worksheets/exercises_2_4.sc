object Exercise {
  // 1
  object Oswald {
    val colour: String = "Black"
    val food: String = "Milk"
  }
  object Henderson {
    val colour: String = "Ginger"
    val food: String = "Chips"
  }
  object Quentin {
    val colour: String = "Tabby and white"
    val food: String = "Curry"
  }
  // 2
  object calc {
    def square(x: Double): Double = x * x
    def cube(x: Double): Double = x * square(x)
  }
  // 3
  object calc2 {
    def square(x: Double) = x * x
    def cube(x: Double) = x * square(x)
    def square(x: Int) = x * x
    def cube(x: Int) = x * square(x)
  }
  // 5
  object person {
    val firstName = "Etienne"
    val lastName = "Peiniau"

  }
  object alien {
    def greet(p: person.type) =
      "Greetings, " + p.firstName + " " + p.lastName
  }
  alien.greet(person)
}



