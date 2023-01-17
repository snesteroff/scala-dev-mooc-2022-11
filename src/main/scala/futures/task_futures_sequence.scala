package futures

import HomeworksUtils.TaskSyntax


import scala.::
import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  def main(arg: Array[String]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val li = Future.successful(5)

    val ex1 = new Exception("ex1")
    val ex2 = new Exception("ex2")
    val failed1 = Future.failed(ex1)
    val failed2 = Future.failed(ex2)

    // , Future.failed(new Exception())
    val f = fullSequence(List(
      Future {
        new RuntimeException()
      }, li, Future.successful(1),
      Future {
        new NullPointerException()
      },
      Future.successful(2), Future {
        Thread.sleep(1000);
        6
      },
      failed2, failed1
    ))

    f.foreach { x =>
      println(x)
    }
    Thread.sleep(3000)
  }

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    futures.foldLeft(Future.successful((List.empty[A], List.empty[Throwable]))) {
      (fr, fa) =>

        fr.zipWith(fa.transform {
          t =>
            t match {
              case Success(s) => Success(s)
              case Failure(ff) => Success(ff.asInstanceOf[A])
            }
        })((t, a) => a match {
          case th: Throwable => (t._1, t._2 :+ th)
          case _ => (t._1 :+ a, t._2)
        })(ex)

    }


}
