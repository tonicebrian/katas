package gameoflife

object Main {
  def main(args:Array[String]){
    val filename = args(0)
    var board : GameOfLife.Board = Nil
    if(args.length > 1) {
      // Means that we want random boards
      val h = 50
      val v = 50
      board = (for (i <- 0 until h) yield
                 (for (j <- 0 until v) yield
                   scala.util.Random.nextBoolean()).toList).toList
    } else {
      board = GameOfLife.parseBoardFromFile(filename)
    }
    while (board.map(line => line.foldLeft(false)(_||_)).foldLeft(false)(_||_)){
      println("")
      printBoard(board)
      board = GameOfLife.evolve(board)
      Thread.sleep(500)
    }
  }

  def printBoard(board:List[List[Boolean]]) = {
    board.map({
      line => {
        line.map(b=>if (b){print('*')}else {print('.')})
        println("")
      }
    })
  }
}