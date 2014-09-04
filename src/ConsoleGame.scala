import Game._

object ConsoleGame {

  // generate random board
  def initial = new State(addRandom(addRandom(EMPTY_BOARD)), 0)

  var current : State = initial

  def printState(state : State) {
    println(s"Score ${state.score}")
    println()
    printBoard(state.board)
  }

  def printBoard(board : Board) {
    var index = 0
    while(index < (SIZE*SIZE)) {
      print(f"${board.at(index)}%4d ")
      index+=1
      if (index%SIZE==0) println()
    }
  }

  def doMove(direction : Direction) {
    current = move(direction, current) match {
      case Success(state) =>
        printState(state)
        state
      case Win(state) =>
        println("You won! Congrats.")
        printState(state)
        println()
        val newGame = initial
        println("New game")
        printState(newGame)
        newGame
      case Loss(state) =>
        println("You lost! Sorry.")
        printState(state)
        println()
        val newGame = initial
        println("New game")
        printState(newGame)
        newGame
      case Forbidden =>
        println("Illegal move")
        current
    }
  }

  def up {
    doMove(UP)
  }

  def down : Unit = {
    doMove(DOWN)
  }

  def left : Unit = {
    doMove(LEFT)
  }

  def right : Unit = {
    doMove(RIGHT)
  }

  def autoplayScore(result : List[(Direction, State)]) : Unit = {
    printState(result.head._2)
    println(s"Finished in ${result.length} moves.")
  }
}
