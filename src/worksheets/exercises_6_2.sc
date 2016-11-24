import scala.collection.immutable.Seq

object Exercises {

  case class Film(name: String, yearOfRelease: Int, imdbRating: Double)

  case class Director(firstName: String, lastName: String, yearOfBirth: Int, films: Seq[Film])

  val memento = Film("Memento", 2000, 8.5)
  val darkKnight = Film("Dark Knight", 2008, 9.0)
  val inception = Film("Inception", 2010, 8.8)
  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = Film("Unforgiven", 1992, 8.3)
  val granTorino = Film("Gran Torino", 2008, 8.2)
  val invictus = Film("Invictus", 2009, 7.4)
  val predator = Film("Predator", 1987, 7.9)
  val dieHard = Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))
  val mcTiernan = Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))
  val nolan = Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))
  val someGuy = Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  nolan.films.map(_.name)
  directors.flatMap(_.films.map(_.name))
  mcTiernan.films.foldLeft(0) { (year, film) =>
    math.min(year, film.yearOfRelease)
  }
  directors.flatMap(_.films).sortBy(_.imdbRating).reverse
  val films: Seq[Film] = directors.flatMap(_.films)
  val average = films.foldLeft(0.0) { (score, film) => score + film.imdbRating } / films.size

  directors.foreach { (d) =>
    d.films.foreach { (f) =>
      println(s"Tonight! ${f.name} by ${d.lastName}!")
    }
  }

  directors
    .flatMap(director => director.films)
    .sortWith((a, b) => a.yearOfRelease < b.yearOfRelease)
    .headOption

  def smallest(seq: Seq[Int]): Int = seq.foldLeft(Int.MaxValue)(math.min)

  def unique(seq: Seq[Int]): Seq[Int] = seq.foldLeft(Seq[Int]())((seq: Seq[Int], current: Int) => if (seq.contains(current)) seq else current +: seq)

  def reverse[A](seq: Seq[A]): Seq[A] = seq.foldLeft(Seq.empty[A])((seq: Seq[A], current: A) =>  current +: seq)

  def map[A, B](seq: Seq[A], f: A => B): Seq[B] = seq.foldRight(Seq.empty[B])((elt, seq) =>  f(elt) +: seq)

  def foldLeft[A, B](seq: Seq[A], zero: B, f: (B, A) => B): B = {
    var result = zero
    seq.foreach { elt => result = f(result, elt) }
    result
  }

}