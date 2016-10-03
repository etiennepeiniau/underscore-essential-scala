object Exercice {

  class Person(val firstName: String, val lastName: String) {
    def name = firstName + " " + lastName
  }

  object Person {
    def apply(name: String) = {
      val parts = name.split(" ")
      new Person(parts(0), parts(1))
    }
  }

  Person.apply("John Doe").firstName
  Person("John Doe").firstName

  class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
    def name = firstName + " " + lastName
  }

  object Director {
    def apply(firstName: String, lastName: String, yearOfBirth: Int): Director = new Director(firstName, lastName, yearOfBirth)

    def older(director1: Director, director2: Director) = if (director1.yearOfBirth > director2.yearOfBirth) director2 else director1
  }

  class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
    def directorsAge = yearOfRelease - director.yearOfBirth

    def isDirectedBy(director: Director) = director == this.director

    def copy(name: String = this.name, yearOfRelease: Int = this.yearOfRelease, imdbRating: Double = this.imdbRating, director: Director = this.director) =
      new Film(name, yearOfRelease, imdbRating, director)
  }

  object Film {
    def apply(name: String, yearOfRelease: Int, imdbRating: Double, director: Director): Film = new Film(name, yearOfRelease, imdbRating, director)

    def newer(film1: Film, film2: Film): Film = if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

    def highestRating(film1 : Film, film2 : Film) = if(film1.imdbRating > film2.imdbRating) film1 else film2

    def oldestDirectorAtTheTime(film1 : Film, film2 : Film) = Director.older(film1.director, film2.director)
  }

}