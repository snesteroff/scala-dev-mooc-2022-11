package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant

import scala.language.postfixOps



/**
 * referential transparency
 */


 object referential_transparency{

  // Test 2
  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }



  def factRec(n: Int): Int =
    if( n <= 0) 1 else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(_n: Int, acc: Int): Int =
      if(_n <= 1) acc
      else loop(_n - 1, _n * acc)

    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */
  def fibbonacci(n: Int): Int = {
    if (n < 0) return 0

    @tailrec
    def go(n1: Int, acc1: Int, acc2: Int): Int = {
      if (n1 >= n) acc2 else {
        go(n1 + 1, acc1 + acc2, acc1)
      }
    }

    go(0, 1, 0)
  }

}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = {
     val cons: Consumer = ???
     val stream: LazyList[Record] = cons.subscribe("request")
     stream.foreach{ r =>
       val req = Request.parse(r.value)
       // save to DB
     }
   }

   def createSubscription[T](topic: String)(f: LazyList[Record] => T): T = {
     val cons: Consumer = ???
     val stream = cons.subscribe(topic)
     f(stream)
   }

  def createRequestSubscription2() = createSubscription("request"){ l =>
    l.foreach{ r =>
      val req = Request.parse(r.value)
      // save to DB
    }
  }

  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }


  def doomy(i: Int): Int = {
    Thread.sleep(1000)
    i + 1
  }

  doomy(1) // 2
  val r1 = logRunningTime(doomy)
  r1(1) //2



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  
  lazy val isEven: Int => Boolean = not(isOdd)





  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val p: Int => Int = partial(2, sum)

  p(3) // 5


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Covariant - animal родитель dog, Option[Animal] родитель Option[Dog]



  // Contravariant - animal родитель dog, Option[Dog] родитель Option[Animal]

  // Invariant - нет отношений

  // Вопрос вариантности

  sealed trait Option[+T]{

    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Option.Some(v) => print(v)
      case _ => ()
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](b: Option[B]): Option[(T, B)] = (this, b) match {
      case (Option.Some(v), Option.Some(w)) => Option.Some((v, w))
      case _ => Option.None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(cond: T => Boolean): Option[T] = this match {
      case some@Option.Some(t) => if (cond(t)) some else Option.None
      case _ => Option.None
    }
  }


  object Option {
    case class Some[T](v: T) extends Option[T]
    case object None extends Option[Nothing]

    def apply[T](v: T): Option[T] = Some(v)
  }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

   trait List[+T] {
     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */
     def ::[TT >: T](elem: TT): List[TT] = List.::(elem, this)

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */
      def mkString(sep: String = ""): String = {
        @tailrec
        def go(list: List[T], acc: String): String = list match {
          case List.Nil => acc
          case List.::(h, t) => {
            val ss = if (acc.isEmpty) "" else sep
            go(t, acc + ss + h.toString)
          }
        }
        go(this, "")
      }

     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse(): List[T] = {
       @tailrec
       def go(list: List[T], acc: List[T] = List.Nil): List[T] =
         list match {
           case List.Nil => acc
           case List.::(h, t) => go(t, h :: acc)
         }
       go(this)
     }
     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */

     def map[B](f: T => B): List[B] = flatMap(t => List(f(t)))

     def ++[TT >: T](addList: List[TT]): List[TT] = this match {
       case List.::(h, t) => h :: (t ++ addList)
       case _ => addList
     }

     def flatMap[B](f: T => List[B]): List[B] = this match {
       case List.::(h, t) => f(h) ++ t.flatMap(f)
       case _ => List.Nil
     }


     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
      def filter(f: T => Boolean): List[T] = this match {
        case List.::(h, t) => if (f(h)) h :: t.filter(f) else t.filter(f)
        case _ => List.Nil
      }


   }

   object List {
     case class ::[A](head: A, tail: List[A]) extends List[A]
     case object Nil extends List[Nothing]

     /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
      def incList(list: List[Int]): List[Int] = list.map( _ + 1 )

     /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
     def shoutString(list: List[String]): List[String] = list.map( "!" + _ )

     /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */
     def apply[A](v: A*): List[A] =
       if(v.isEmpty) Nil else ::(v.head, apply(v.tail:_*))
   }

   List(1, 2, 3)
 }