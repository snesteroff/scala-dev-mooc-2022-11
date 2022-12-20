package module1

import scala.util.Random

class BallsExperiment {
  val blackOrWhite = Random.nextBoolean()

  def isFirstBlackSecondWhite(): Boolean = blackOrWhite
}

object BallTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.fill(count) {

    }
    val countOfExperiments = ???
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}
