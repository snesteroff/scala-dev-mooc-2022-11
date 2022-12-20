import module1.{functions, opt, recursion, referential_transparency, type_system, list}

object Main{


  def main(args: Array[String]): Unit = {

    println(type_system.v.foo())
    println(type_system.v1.foo())
    println("Hello, World!")
    println(recursion.fibbonacci(0))

    val fiveOption = opt.Option.Some(5)
    val empty = opt.Option.None

    val zp = opt.Option.Some("one").zip(opt.Option.Some("two"))
    println(s"Значение класса Option zp = $zp")
    val zp1 = opt.Option.Some("one").zip(opt.Option.None)
    println(s"Значение класса Option zp1 = $zp1")

    fiveOption.printIfAny
    empty.printIfAny
    println()
    println("Lists:")
    val l = list.List(1, 2, 3, 4, 5)
    println(l.reverse)
    println(l.mkString(":"))
    println(l.flatMap(x => list.List(x, x)))
    println(l.map(x => x * 3))

    val s = list.List("a","b", "C", "d")
    println(list.List.shoutString(s))
    println(list.List.incList(l))
    println(l.filter(_ % 2 == 0))
  }

}