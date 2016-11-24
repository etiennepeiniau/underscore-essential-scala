object Exercices {

  sealed trait JsValue {
    def stringify: String
  }

  final case class JsObject(values: Map[String, JsValue]) extends JsValue {
    def stringify = values
      .map { case (name, value) => "\"" + name + "\":" + value.stringify }
      .mkString("{", ",", "}")
  }

  final case class JsString(value: String) extends JsValue {
    def stringify = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
  }

  trait JsWriter[A] {
    def write(value: A): JsValue
  }

  implicit class JsUtil[A](value: A) {
    def toJson(implicit jsWriter: JsWriter[A]): JsValue = jsWriter write value
  }

  import java.util.Date

  sealed trait Visitor {
    def id: String

    def createdAt: Date

    def age: Long = new Date().getTime - createdAt.getTime
  }

  final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

  final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor

  implicit val userJsWriter = new JsWriter[User] {
    override def write(user: User): JsValue =
      JsObject(Map("id" -> JsString(user.id), "email" -> JsString(user.email), "createdAt" -> JsString(user.createdAt.toString)))
  }

  implicit val anonymousJsWriter = new JsWriter[Anonymous] {
    override def write(ano: Anonymous): JsValue =
      JsObject(Map("id" -> JsString(ano.id), "createdAt" -> JsString(ano.createdAt.toString)))
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User => user.toJson
    }
  }

  val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@xample.com", new Date))
  visitors.map(v => v.toJson)

  Anonymous("001", new Date).toJson

}