import Exceptions._
import States._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class Game {
  val printer = new ConsolePrinter
  val ai = new AI(O, this)
  private val grid: Array[State] = Array.fill[State](9)(Empty)

  def getGrid: Array[State] = grid.clone()

  /**
   * играет несколько игр с выбором первого хода и предложением завершить игру
   */
  def playGames(): Future[Unit] = {
    printer.printMessage("Необходимо вводить ход в формате СтолбецСтрока. Например, A1")
      .flatMap(_ => isAiFirst)
      .flatMap {
        case true => play(O, matchMove)
        case false => printer.printGrid(grid.clone()).flatMap(_ => play(X, matchMove))
      }.flatMap {
      case (true, Empty) => printer.printMessage("ничья")
      case (true, state) => printer.printMessage(s"выиграл $state")
    }.flatMap(_ => isGameEnd)
      .flatMap {
        case false =>
          emptyGrid()
          playGames()
        case true => Future.successful()
      }
  }

  /**
   * играет одну игру
   *
   * @param move первый ход
   * @param matchMove функция, определяющая, кто ходит Х, а кто O
   */
  def play(move: State, matchMove: State => Future[Int]): Future[(Boolean, State)] = {
    matchMove(move)
      .flatMap(mov => makeMove(grid, move, mov))
      .flatMap(_ => printer.printGrid(grid.clone()))
      .flatMap(_ => checkWin(grid) match {
        case (false, _) => play(nextMove(move), matchMove)
        case (a, b) => Future {
          (a, b)
        }
      })
      .recoverWith {
        case _: WrongMoveException =>
          printer.printMessage("Эта клетка уже занята или находится за границами поля")
            .flatMap(_ => play(move, matchMove))
        case _: WrongInputException => printer.printMessage("Некорректный формат ввода: необходимо вводить " +
          "ход в формате СтолбецСтрока. Например, A1")
          .flatMap(_ => play(move, matchMove))
        case e => Future.failed(e)
      }
  }

  private def emptyGrid(): Unit = {
    for (i <- grid.indices)
      grid(i) = Empty
  }

  private def isAiFirst: Future[Boolean] = {
    printer.isAiFirst.recoverWith {
      case _: WrongInputException =>
        printer.printMessage("необходимо ввести 0 или 1")
        isAiFirst
      case e => Future.failed(e)
    }
  }

  private def isGameEnd: Future[Boolean] = {
    printer.isGameEnd.recoverWith {
      case _: WrongInputException =>
        printer.printMessage("необходимо ввести 0 или 1")
        isGameEnd
      case e => Future.failed(e)
    }
  }

  private def matchMove(move: State): Future[Int] = {
    move match {
      case X => printer.getMove
      case O => ai.getMove(grid.clone())
    }
  }

  private def makeMove(grid: Array[State], move: State, position: Int): Future[Unit] = {
    Future {
      if (position < 0 || position >= grid.length || grid(position) != Empty)
        throw new WrongMoveException

      grid(position) = move
    }
  }

  def nextMove(currentMove: State): State = {
    if (currentMove == X) O else X
  }

  /**
   * проверяет, завершилсь ли игра ничьей или победой для поля 3*3
   *
   * @return (закончилась ли игра, кто победил(Empty - ничья))
   */
  def checkWin(grid: Array[State]): (Boolean, State) = {
    for (i <- grid.indices by 3) {
      val state = grid(i)
      if (state != Empty && grid(i + 1) == state && grid(i + 2) == state)
        return (true, state)
    }

    for (i <- 0 until 3) {
      val state = grid(i)
      if (state != Empty && grid(i + 3) == state && grid(i + 6) == state)
        return (true, state)
    }

    if (grid(0) != Empty && grid(0) == grid(4) && grid(0) == grid(8))
      return (true, grid(0))

    if (grid(2) != Empty && grid(2) == grid(4) && grid(2) == grid(6))
      return (true, grid(2))

    if (grid.contains(Empty))
      return (false, null)

    (true, Empty)
  }
}
