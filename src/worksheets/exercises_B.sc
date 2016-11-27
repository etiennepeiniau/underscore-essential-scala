import collection.mutable

object Exercises {

  // immutable
  val animals = Seq("cat", "dog", "penguin")
  val allAnimals = "mouse" +: animals :+ "tyrannosaurus"
  val bag = 2 +: allAnimals

  // mutable
  val mutableAnimals = mutable.Seq("cat", "dog", "penguin")
  mutableAnimals(1) = 2

}