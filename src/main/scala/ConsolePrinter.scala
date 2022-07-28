import Exceptions._
import States.State

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.math.sqrt
import scala.io.StdIn._

class ConsolePrinter {
  //сопоставление между буквенным обозначением столбца и его номером
  val charMap = Map('A' -> 0, 'B' -> 1, 'C' -> 2)

  def isAiFirst: Future[Boolean] = {
    Future {
      println("Введите 0, если ИИ будет ходить первым, 1, если человек")
      askYesOrNo
    }
  }

  def isGameEnd: Future[Boolean] ={
    Future{
      println("Введите 0, если хотите закончить игру, 1, если сыграть еще одну")
      askYesOrNo
    }
  }

  private def askYesOrNo ={
    try {
      readLine().toInt match {
        case 0 => true
        case 1 => false
        case _ => throw new WrongInputException
      }
    }
    catch {
      case _: Throwable => throw new WrongInputException
    }
  }

  def printGrid(grid: Array[State]): Future[Unit] = {
    Future {
      val side = sqrt(grid.length).toInt
      var counter = 1
      print("  ")
      charMap.foreach(x => print(s"${x._1} "))
      println()
      for (i <- grid.indices by side) {
        print(counter)
        counter += 1
        for (j <- i until i + side)
          print(s" ${grid(j).toString}")
        println()
      }
      println("----------")
    }
  }

  def getMove: Future[Int] = {
    Future {
      try {
        val input = readLine()
        val column = input(0)
        val row = input(1).toString.toInt
        charMap(column) + (row - 1) * charMap.size
      }
      catch {
        case _: Throwable => throw new WrongInputException
      }
    }
  }

  def printMessage(message: String): Future[Unit] = {
    Future {
      println(message)
    }
  }
}
