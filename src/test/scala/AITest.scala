import States._
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec

import scala.concurrent.Future
import scala.util.Random

class AITest extends AsyncFlatSpec {

  "ai vs ai" should "play draw" in {
    val game = new Game
    val ai1 = new AI(X, game)
    val ai2 = new AI(O, game)

    def matchMove(move: State): Future[Int] = {
      move match {
        case X => ai1.getMove(game.getGrid.clone())
        case O => ai2.getMove(game.getGrid.clone())
      }
    }

    game.play(O, matchMove).map(x => assert(x._1 && x._2 == Empty))
  }

  "ai vs random" should "win or draw" in {
    def makeRandomMove(grid: Array[State]) = {
      Future {
        val emptySlots = grid.indices.filter(i => grid(i) == Empty)
        val random = new Random
        emptySlots(random.nextInt(emptySlots.length))
      }
    }

    def playGameAndAssert(): Future[Assertion] = {
      val game = new Game
      val ai1 = new AI(O, game)

      def matchMove(move: State): Future[Int] = {
        move match {
          case X => makeRandomMove(game.getGrid.clone())
          case O => ai1.getMove(game.getGrid.clone())
        }
      }

      def isGoodResult(res: (Boolean, State)): Boolean = {
        if (res._1 && (res._2 == O || res._2 == Empty))
          return true
        false
      }

      game.play(X, matchMove).map(x => assert(isGoodResult(x)))
    }

    var futures = List[Future[Assertion]]()

    for (_ <- 0 to 30) {
      futures = playGameAndAssert() :: futures
    }

    def assertAll(futures: List[Future[Assertion]]): Future[Assertion] =
      Future.sequence(futures).map(_.foreach(a => a)).map(_ => succeed)

    assertAll(futures)
  }
}