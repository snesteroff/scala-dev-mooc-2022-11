package scala3

//import scala.language.postfixOps

object Scala3:
  @main def parseMain(): Unit =
    val str = "1,test1,true\n2,test2,false,\n3,test3,true"
    val res = new ParserWithGivenParam().readCSVString(str)
    res.foreach(println(_))

  class ParserWithGivenParam:
    var parser =
      for
        field1 <- intField
        field2 <- strField
        field3 <- boolField
      yield TestClass(field1, field2, field3)

    def readCSVString(str: String) = str.split("\n").map(parser.parse)

    def intField(using ctx: FieldConversion[String, Int]) = strField.map(ctx.convert(_))

    def boolField(using ctx: FieldConversion[String, Boolean]) = strField.map(ctx.convert(_))

    def strField = MonadParser[String, String] { str =>
      val idx = str.indexOf(",")
      if idx > -1 then
        (str.substring(0, idx), str.substring(idx + 1))
      else
        (str, "")
    }

    trait FieldConversion[String, A]:
      def convert(x: String): A

    given intFieldConversion: FieldConversion[String, Int] with
      def convert(x: String): Int = x.toInt

    given boolFieldConversion: FieldConversion[String, Boolean] with
      def convert(x: String): Boolean = x.toBoolean

    class MonadParser[T, Src](private val p: Src => (T, Src)):
      def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
        MonadParser { src =>
          val (word, rest) = p(src)
          f(word).p(rest)
        }

      def map[M](f: T => M): MonadParser[M, Src] =
        MonadParser { src =>
          val (word, rest) = p(src)
          (f(word), rest)
        }

      def parse(src: Src): T = p(src)._1

    case class TestClass(i: Int, s: String, b: Boolean)





