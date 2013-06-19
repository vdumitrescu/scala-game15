package com.bebelici.game15

class Game(start: Vector[Vector[Int]]) {

  val size = start.length
  type Board = Map[(Int, Int), Int]
  
  val initialBoard = (for {
    row <- 0 until size
    col <- 0 until size
  } yield (row, col) -> start(row)(col)
  ).toMap

  val holePosition = (for {
    row <- 0 until size
    col <- 0 until size
    if start(row)(col) == 0
  } yield (row, col)).head
  
  val solutionBoard = ( for {
    row <- 0 until size
    col <- 0 until size    
  } yield (row, col) -> (row * 3 + col + 1)
  ).toMap updated ((size-1, size-1), 0) 
  
  trait Move
  case class North extends Move
  case class South extends Move
  case class East  extends Move
  case class West  extends Move

  val moves = List(North(), South(), East(), West())
  
  class Position(val row: Int, val col: Int) {
    
    def next(move: Move): Position = move match {
      case South() =>
        if (row > 0) new Position(row - 1, col)
        else null
      case North() =>
        if (row < size - 1) new Position(row + 1, col)
        else null
      case East() =>
        if (col > 0) new Position(row, col - 1)
        else null
      case West() =>
        if (col < size - 1) new Position(row, col + 1)
        else null
    } 
  }
  

  class State(val board: Board, hole: Position, val history: List[Move]) {
    
    def next(move: Move): State = {
      val newHole = hole.next(move)
      if (newHole == null) null
      else {
        val piece = board((newHole.row, newHole.col))
        val newBoard = board updated ((newHole.row, newHole.col), 0) updated ((hole.row, hole.col), piece)
        new State(newBoard, newHole, move::history)
      }
    }
    
    def nextStates: Set[State] = (for {
      m <- moves
      if this.next(m) != null
    } yield this.next(m)
    ).toSet
    
    override def toString = ( for {
      r <- 0 until size
      c <- 0 until size
    } yield board((r, c)) 
    ) mkString " "
  }
  
  val initialHole = new Position(holePosition._1, holePosition._2)
  val initialState = new State(initialBoard, initialHole, List())
  
  def generate(toAnalyze: Set[State], analyzed: Set[State]): Stream[Set[State]] = {
    if (toAnalyze.isEmpty) Stream.empty
    else {
      val children = for {
        state <- toAnalyze
        next <- state.nextStates
        if (!analyzed.contains(next))
      } yield next
      toAnalyze #:: generate(children, analyzed ++ children)
    }
  }
  
  val stateSets = generate(Set(initialState), Set())
  
  def solution = for {
    stateSet <- stateSets
    state <- stateSet
    if state.board.equals(solutionBoard)
  } yield state.history
}