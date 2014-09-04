import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Game {

  // all boards are square
  def SIZE  = 4
  // probability of 4
  def FOUR_CHANCE = 0.1
  // winning cell value
  def WIN_CELL_VALUE = 2048

  class Board(val data : Array[Int]) {
    def at(x : Int, y : Int) : Int = {
      data(x + y * SIZE)
    }
    def at(i : Int) : Int = {
      data(i)
    }
  }

  val EMPTY_BOARD = new Board(Array.fill[Int](SIZE*SIZE)(0))

  class State(val board : Board, val score : Int)

  class Move(val state : State, val dir : Direction)

  // move result
  abstract sealed class MoveResult
  // move is forbidden i.e. no shift will happen
  case object Forbidden extends MoveResult
  // move succeeded and new state is enclosed
  final case class Success(state : State) extends MoveResult
  // we won the game (built 2048 cell)
  final case class Win(state : State) extends MoveResult
  // we lost the game (no more moves available)
  final case class Loss(state : State) extends MoveResult

  // define direction
  class Direction(val start : Int, val iDelta : Int, val oDelta : Int, val name : String) {
    override def toString = name
  }

  def SQUARE = (SIZE * SIZE) - 1

  val RIGHT = new Direction(SQUARE,    -1,       0, "RIGHT" )
  val UP    = new Direction(     0,  SIZE, -SQUARE, "UP"    )
  val LEFT  = new Direction(     0,     1,       0, "LEFT"  )
  val DOWN  = new Direction(SQUARE, -SIZE,  SQUARE, "DOWN"  )

  // perform requested move. if dir is illegal move is not performed
  def shift(dir : Direction, state : State) : Option[State] = {
    val newBoard = Array.fill[Int](SIZE*SIZE)(0)
    var newScore = state.score
    var moveHappened = false
    // fold
    var index = dir.start
    var outer = 0
    while(outer<SIZE) {
      var prev = -1
      var prevIndex = index - dir.iDelta
      var inner = 0
      while(inner<SIZE) {
        val c = state.board.at(index)
        if (c>0) {
          if (c==prev) {
            // merge
            newBoard(prevIndex) = c*2
            prev = 0
            prevIndex += dir.iDelta
            newScore += c*2
            moveHappened = true
          } else if (prev==0) {
            // slide
            newBoard(prevIndex) = c
            prev = c
            moveHappened = true
          } else {
            // slide to prev + 1
            newBoard(prevIndex + dir.iDelta) = c
            prev=c
            prevIndex=prevIndex + dir.iDelta
            if (prevIndex!=index) moveHappened = true
          }
        } else {
          // empty square
          if (prev== -1) {
            // became new prev 0
            prevIndex=index
            prev = c
          }
        }

        index += dir.iDelta
        inner+=1
      }
      index += dir.oDelta
      outer+=1
    }

    if (moveHappened) Some(new State(new Board(newBoard), newScore)) else None
  }

  def emptyCells(board : Board) = for {
    i <- 0 to SQUARE
    if board.at(i)==0
  } yield i

  def move(dir : Direction, state : State) : MoveResult = {
    shift(dir, state) match {
      case Some(newState) =>
        gameState(new State(addRandom(newState.board), newState.score))
      case None => Forbidden
    }
  }

  val rnd = new Random()

  // check if we won or lost the game
  def gameState(state : State) : MoveResult = {
    if (state.board.data.contains(WIN_CELL_VALUE)) Win(state)
    else if (state.board.data.contains(0) || hasAdjacentCells(state.board)) Success(state)
    else Loss(state)
  }

  def hasAdjacentCells(board : Board) : Boolean = {
    var result = false
    var y = 0
    var x = 0
    var index = 0
    while(!result && y<(SIZE-1)) {
      x=0
      // main field iteration right and down
      while(!result && x<(SIZE-1)) {
        result = result || (board.at(index)==board.at(index+1)) || (board.at(index)==board.at(index+SIZE))
        index = index + 1
        x=x+1
      }
      // last column check only down
      result = result || (board.at(index)==board.at(index+SIZE))
      index = index + 1
      y = y + 1
    }
    // last row iteration only right
    x=0
    while(!result && x<(SIZE-1)) {
      result = result || (board.at(index)==board.at(index+1))
      index = index + 1
      x=x+1
    }
    result
  }

  def addRandom(board : Board) : Board = {
    val empty = emptyCells(board).toList
    val cell = rnd.nextInt(empty.length)
    addCell(board, empty(cell), if (rnd.nextFloat() > FOUR_CHANCE) 2 else 4)
  }

  def addCell(board : Board, index : Int, value : Int) : Board = {
    val newBoard = Array.fill[Int](SIZE*SIZE)(0)
    Array.copy(board.data, 0, newBoard, 0, SIZE*SIZE)
    newBoard(index) = value
    new Board(newBoard)
  }

  def fromCells(cells : Seq[(Int, Int, Int)]) : Board = {
    cells.foldLeft(EMPTY_BOARD)((b,p)=>addCell(b,p._1+p._2*SIZE,p._3))
  }

  def fromSequence(cells : Seq[Int]) : Board = {
    new Board(cells.foldRight(new mutable.ArrayBuilder.ofInt)((v,b)=>b+=v).result())
  }

}
