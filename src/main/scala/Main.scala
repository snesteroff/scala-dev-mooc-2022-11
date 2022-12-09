import module1.{functions, type_system}

object Main{


  def main(args: Array[String]): Unit = {

    println(type_system.v.foo())
    println("Hello, World!")
  }

}