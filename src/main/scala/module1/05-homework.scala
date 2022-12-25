package module1

import scala.util.Random

class BallsExperiment {
  val binOfBalls: Vector[Boolean] = Vector(true, true, true, false, false, false)
  val sizeOfBin = binOfBalls.size
  def isFirstBlackSecondWhite(): Boolean = {
    val i = Random.nextInt(sizeOfBin)
    var j: Int = -1
    do {
      j = Random.nextInt(sizeOfBin)
    } while (j == i)
    if (!binOfBalls(i) && binOfBalls(j)) true else false
  }
}

object BallTest {
  def main(args: Array[String]): Unit = {
    val count = 100000
    val listOfExperiments: List[BallsExperiment] = List.fill(count) {
      new BallsExperiment
    }
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}
