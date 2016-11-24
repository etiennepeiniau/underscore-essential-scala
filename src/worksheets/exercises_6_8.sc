object Exercises {

  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages = Map(
    "Alice" -> 20,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60)

  val favoriteColors = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow")

  val favoriteLolcats = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat")

  def favoriteColor(name: String): Option[String] = favoriteColors get name

  def favoriteColorWithDefault(name: String): String = favoriteColors.getOrElse(name, "beige")

  def printColors(): Unit = people.foreach { name => println(s"$name's favorite color is ${favoriteColorWithDefault(name)}") }

  def lookup[T](name: String, map: Map[String, T]): Option[T] = map get name

  val oldest: Option[String] =
    people.foldLeft(Option.empty[String]) { (older, person) =>
      if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) {
        Some(person)
      } else {
        older
      }
    }

  val favorite: Option[String] =
    for {
      oldest <- oldest
      color <- favoriteColors.get(oldest)
    } yield color

  def union[T](s1: Set[T], s2: Set[T]): Set[T] = s1.foldLeft(s2)((union, current) => union + current)

  val set1 = Set(1, 3, 5)
  val set2 = Set(5, 7, 9)

  union(set1, set2)

  def union[A](map1: Map[A, Int], map2: Map[A, Int]): Map[A, Int] = {
    map1.foldLeft(map2) { (map, elt) =>
      val (k, v) = elt
      val newV = map.get(k).map(_ + v).getOrElse(v)
      map + (k -> newV)
    }
  }

  val addAges = Map(
    "Alice" -> 10,
    "Bob" -> 20,
    "Thomas" -> 50)

  union(ages, addAges)

  def union[A, B](map1: Map[A, B], map2: Map[A, B], add: (B, B) => B): Map[A, B] = {
    map1.foldLeft(map2) { (map, elt) =>
      val (k, v) = elt
      val newV = map.get(k).map(v2 => add(v, v2)).getOrElse(v)
      map + (k -> newV)
    }
  }

}