import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game
    Await.result(game.playGames(), Duration.Inf)
  }
}
