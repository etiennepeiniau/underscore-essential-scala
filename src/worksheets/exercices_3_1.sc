object Exercice {

  class Cat(val color: String, val food: String, val breed: String = "Common")

  val oswald = new Cat(color = "Black", food = "Milk")
  val henderson = new Cat(color = "Ginger", food = "Chips")
  val quentin = new Cat(color = "Tabby and white", food = "Curry")

  object ChipShop {
    def willServe(cat: Cat): Boolean = cat.food == "Chips"
  }

}