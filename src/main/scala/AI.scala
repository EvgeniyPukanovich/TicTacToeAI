import States._
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AI(move: State, game: Game) {
  //кэш, чтобы заново не высчитывать ход, если он уже был высчитан
  private val moveMap: ConcurrentHashMap[Seq[State], Integer] = new ConcurrentHashMap

  def getMove(grid: Array[State]): Future[Int] = {
    Future {
      val bestPosition: Int = moveMap.get(grid.toSeq) match {
        case null => val position = GetBestPosition(grid)
          moveMap.put(grid.toSeq, position)
          position
        case position => position
      }
      bestPosition
    }
  }

  private def GetBestPosition(grid: Array[State]) = {
    val emptySlots = getEmptySlots(grid)

    var evaluations = List[(Int, Int)]()
    for (slot <- emptySlots) {
      val gr = grid.clone()
      gr(slot) = move
      val eval = maxMin(gr, game.nextMove(move))
      evaluations = (slot, eval) :: evaluations
    }
    val pos = evaluations.maxBy(_._2)
    pos._1
  }

  //алгоритм MaxMin для оценки хода
  //предполагается, что противник играет оптимально
  //возвращает 0, если ход приводит к ничьей, 1 - к выигрышу, -1 - к проигрышу
  private def maxMin(grid: Array[State], currentMove: State): Int = {
    game.checkWin(grid) match {
      case (true, Empty) => 0
      case (true, winner) if winner == move => 1
      case (true, winner) if winner != move => -1
      case _ =>
        val evaluations = getEmptySlots(grid)
          .map(i => {
            val gr = grid.clone()
            gr(i) = currentMove
            gr
          }).map(grid => maxMin(grid, game.nextMove(currentMove)))

        if (currentMove == move)
          evaluations.max
        else
          evaluations.min
    }
  }

  private def getEmptySlots(grid: Array[State]) = {
    grid.indices.filter(i => grid(i) == Empty)
  }
}
