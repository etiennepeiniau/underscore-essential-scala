object Exercises {

  case class Cat(color: String, food: String)

  case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
    def name = s"$firstName $lastName"
  }

  object Director {
    def older(director1: Director, director2: Director) = if (director1.yearOfBirth > director2.yearOfBirth) director2 else director1
  }

  case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) {
    def directorsAge = yearOfRelease - director.yearOfBirth

    def isDirectedBy(director: Director) = director == this.director
  }

  object Film {
    def newer(film1: Film, film2: Film): Film = if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

    def highestRating(film1: Film, film2: Film) = if (film1.imdbRating > film2.imdbRating) film1 else film2

    def oldestDirectorAtTheTime(film1: Film, film2: Film) = Director.older(film1.director, film2.director)
  }

  case class Counter(count : Int = 0) {
    def dec: Counter = dec()

    def inc: Counter = inc()

    def inc(value: Int = 1) = copy(count = count + value)

    def dec(value: Int = 1) = copy(count = count - value)

    def adjust(adder: Adder) = copy(count = adder(count))
  }

  class Adder(amount: Int) {
    def apply(in: Int) = in + amount
  }


  case class Person(firstName: String, lastName: String) {
    def name = firstName + " " + lastName
  }

  object Person {
    def apply(name: String) = {
      val parts = name.split(" ")
      new Person(parts(0), parts(1))
    }
  }

}