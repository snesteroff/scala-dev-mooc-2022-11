package module2

import module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }

  // 1
  trait JsonWriter[T]{
    def write(v: T): JsValue
  }

  val result2 = List(1, 2, 3).filter(_ === 4)

  implicit class JsonSyntax[T](v: T){
    def toJson(implicit w: JsonWriter[T]): JsValue = w.write(v)
  }

  def toJson[T: JsonWriter](v: T): JsValue = {

    JsonWriter[T].write(v)
  }

  toJson("vfbfb")
  toJson(12)
  toJson(Option(12))
  toJson(Option("fbfbfbf"))

  "fbfbf".toJson
  10.toJson
  Option(10).toJson
  Option("scdvv").toJson

  def _max[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if (ordering.less(a, b)) b else a

  // 1 компонент
  trait Ordering[T] {
    def less(a: T, b: T): Boolean
  }

  // 3 implicit parameter

  trait Eq[T] {
    def ===(a: T, b: T): Boolean
  }

  //  _max(5, 10) // 10
  //  _max("ab", "abcd") // abcd


  // ===
  val result = List("a", "b", "c").filter(str => str === "")

  object JsonWriter {
    def apply[T](implicit w: JsonWriter[T]): JsonWriter[T] = w

    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }

    implicit val strJsValue: JsonWriter[String] = from[String](JsString)

    implicit val intJsValue: JsonWriter[Int] = from[Int](JsNumber(_))

    implicit def optJSValue[T](implicit w: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
      case Some(value) => w.write(value)
      case None => JsNull
    }


  }

  // 2 значения
  object Ordering {

    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }

    //    implicit val intOrdering = Ordering.from[Int]((a, b) => a < b)
    //
    //    implicit val strOrdering = Ordering.from[String]((a, b) => a < b)
  }

  object Eq {

    def apply[T](): Eq[T] = ???

    implicit val eqStr: Eq[String] = new Eq[String] {
      override def ===(a: String, b: String): Boolean = a == b
    }
    implicit val eqInt: Eq[Int] = new Eq[Int] {
      override def ===(a: Int, b: Int): Boolean = a == b
    }
  }

  implicit class EqSyntax[T](a: T){
    def ===(b: T)(implicit eq: Eq[T]): Boolean = eq.===(a, b)
  }


}
