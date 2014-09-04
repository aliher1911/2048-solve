import Game._

/**
 *
 */
object Solver {
  // game and search parameters
  val newCellValues = List((0.9,2), (0.1,4))
  val directions = List(UP, RIGHT, DOWN, LEFT)
  val searchDepth = 3

  // statistics
  val statisticsIntervalMillis = 10000
  var fieldsEvaluated = 0L
  var movesMade = 0L
  var startTime = 0L
  var lastReport = 0L

  // all possible moves from the state in direction
  def possibleMoves(direction : Direction, state : State) : Iterable[(Double, State)] = {
    shift(direction, state) match {
      case None => List()
      case Some(s) =>
        val moves = emptyCells(s.board)
        for {
          c <- moves
          v <- newCellValues
        } yield (v._1/moves.length, new State(addCell(s.board, c, v._2), s.score))
    }
  }

  def freeSpaceEvaluator(state : State) : Double = state.board.data.count(_==0)

  // find solution - find best dir
  def findSolution(state : State, depth : Int, evaluator : State => Double) : Direction = {
    val evaluation = findDirectionScores(state, depth, evaluator)
    val (dir : Direction, score : Double) = evaluation.maxBy(_._2)
    if (score>0 || depth==0) dir
    else findSolution(state, depth-1, evaluator) // no win with current depth, try less
  }

  // find score for state after the move was performed
  // if last stage, calculate score
  // if no directions available return 0
  def findStateScore(state : State, depth : Int, evaluator : State => Double) : Double = {
    if (depth==0) {
      fieldsEvaluated += 1
      evaluator(state)
    } else {
      val dirs = findDirectionScores(state, depth - 1, evaluator)
      if (dirs.isEmpty) 0 else dirs.maxBy(_._2)._2
    }
  }

  // find scores for all directions
  def findDirectionScores(state : State, depth : Int, evaluator : State => Double) : Iterable[(Direction, Double)] =
    // (dir score == -1 if move is not legal and we filter it after)
    (for (dir <- directions) yield (dir, possibleMoves(dir, state).foldLeft[Double](-1)((a, s) => {
      (if (a<0) 0 else a) + s._1 * findStateScore(s._2, depth, evaluator)
    }))).filter(_._2>=0)

  def playGame(state : State) : List[(Direction, State)] = {
    fieldsEvaluated = 0
    movesMade = 0
    startTime = System.currentTimeMillis()
    lastReport = startTime
    playByItself(state, List())
  }

  // play game and build path of this game as a result
  def playByItself(state : State, history : List[(Direction, State)]) : List[(Direction, State)] = {
    val solution = findSolution(state, searchDepth, freeSpaceEvaluator)
    movesMade += 1
    move(solution, state) match {
      case Success(nextState) =>
        printStats()
        playByItself(nextState, (solution, nextState) :: history)
      case Win(nextState) =>
        (solution, nextState) :: history
      case Loss(nextState) =>
        println("Lost")
        (solution, nextState) :: history
      case Forbidden =>
        println("Internal error")
        history
    }
  }

  def printStats() : Unit = {
    if ((System.currentTimeMillis()-lastReport)>statisticsIntervalMillis) {
      val timeTaken = (System.currentTimeMillis()-startTime)/1000.0
      println(s"Time             ${timeTaken/60} minutes")
      println(s"Moves made       $movesMade")
      println(s"Fields evaluated $fieldsEvaluated")
      println(s"Performance      ${movesMade/timeTaken} moves/sec")
      println(s"                 ${fieldsEvaluated/timeTaken} fields/sec")
      lastReport = System.currentTimeMillis()
    }
  }
}
