object Exercises {

  object Positive {

    def unapply(i: Int): Option[Int] = if (i > 0) Some(i) else None

  }

  assert(
    "No" ==
      (0 match {
        case Positive(_) => "Yes"
        case _ => "No"
      })
  )
  assert(
    "Yes" ==
      (42 match {
        case Positive(_) => "Yes"
        case _ => "No"
      })
  )

  object Titlecase {

    def unapply(s : String): Option[String] =
      Some(s.split(" ").map {
        case "" => ""
        case word => word.substring(0,1).toUpperCase + word.substring(1)
      }.mkString(" "))

  }

  assert(
    "Sir Lord Doctor David Gurnell" ==
      ("sir lord doctor david gurnell" match {
        case Titlecase(str) => str
      })
  )

}