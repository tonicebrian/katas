package gameoflife

import util.parsing.combinator.Parsers

object GameOfLife {

  type Board = List[List[Boolean]]

  def parseBoardFromFile(path:String):Board = {
    val lines = io.Source.fromFile(path).mkString.split('\n')
    lines.map(_.toCharArray.map(c => if (c=='.') {false} else {true}).toList).toList
  }

  def evolve(board:Board):Board = {
    val (ls,cs) = (board.length,board.head.length)
    val (innerLs,innerCs) = ((1 to ls-2),(1 to cs-2))
    val dummyLine = for (i <- 0 until cs) yield false
    val dummyCel = false
    val newKernel:List[List[Boolean]] =
                    (for (l <- innerLs) yield {
                      val innerList = (for(c <- innerCs) yield eval(board,l,c)).toList
                      dummyCel::innerList++List(dummyCel)
                    }).toList

    (dummyLine.toList::newKernel)++List(dummyLine.toList)
  }

  private def eval(board:Board, l:Int, c:Int):Boolean = {
      val n = aliveNeighbours(board,l,c)
      if (n < 2 || n>3) {
        false
      } else if (n == 3) {
        true
      } else {
        board(l)(c)
      }
  }

  private def aliveNeighbours(b:Board,l:Int,c:Int):Int = {
    val (nw,n,ne) = (b(l-1)(c-1),b(l-1)(c),b(l-1)(c+1))
    val (w,e) = (b(l)(c-1),b(l)(c+1))
    val (sw,s,se) = (b(l+1)(c-1),b(l+1)(c),b(l+1)(c+1))
    List(nw,n,ne,w,e,sw,s,se).filter(x => x).length
  }
}