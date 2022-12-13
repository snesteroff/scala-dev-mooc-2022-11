import module1.{functions, opt, recursion, referential_transparency, type_system}

object Main{


  def main(args: Array[String]): Unit = {

    println(type_system.v.foo())
    println(type_system.v1.foo())
    println("Hello, World!")
    println(recursion.fibonacci(0))

    val fiveOption = opt.Option.Some(5)
    val empty = opt.Option.None

    fiveOption.printIfAny
    empty.printIfAny

  }

}