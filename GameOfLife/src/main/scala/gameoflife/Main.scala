package gameoflife

object Main {
  def main(args:Array[String]){
    val filename = args(0)
    var board : GameOfLife.Board = Nil
    if(filename.equals("random")) {
      // Means that we want random boards
      var h = 50
      var v = 50
      if(args.length == 2) {
        h = args(1).toInt
        v = h
      }
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
