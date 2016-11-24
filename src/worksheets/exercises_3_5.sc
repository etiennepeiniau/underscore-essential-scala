object Exercises {

  case class Cat(color: String, food: String)

  object ChipShop {
    def willServe(cat: Cat) = cat match {
      case Cat(_, "chips") => true
      case Cat(_, _) => false
    }
  }

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

  object Dad {
    def rate(film : Film) : Double = film match {
      case Film(_,_,_,Director("Clint","Eastwood",_)) => 1.0
      case Film(_,_,_,Director("John","McTiernan",_)) => 0.7
      case _ => 0.3
    }
  }

}