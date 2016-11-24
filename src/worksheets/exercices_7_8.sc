object Exercices {

  class IntOps(val i: Int) {
    def yeah(): Unit = times((i) => println("Oh yeah!"))

    def times(f: Int => Unit): Unit = if (i > 0) (1 to i).foreach(f)
  }

  implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

  2.yeah
  3.yeah
  (-1).yeah

  3.times(i => println(s"Look - it's the number $i!"))


}