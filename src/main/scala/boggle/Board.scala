import scala.collection._

object Board {
  def apply(b: IndexedSeq[IndexedSeq[Char]]) = new Board(b)

  //val words = wordsFrom(board)

  val dictWords = List("the", "tot", "his", "the", "to", "this", "fuck", "face")
  def getWords = { 
    import scala.io._
    val f = Source.fromFile("/usr/share/dict/words")
    f.getLines().map(_.toLowerCase()).toList
  }
  val dict = Trie(getWords.map(_.toList))



  class Board(val board: IndexedSeq[IndexedSeq[Char]]) {
    val ymax = board(0).length - 1
    val xmax = board.length - 1
    

    case class Node(x: Int, y: Int, c: Char) {
      def char = board(x)(y)
      def neighbors(): List[Node] = {
        val moves = List(-1, 0, 1)
        val neighborCoords = moves flatMap { a => moves map { b => (a, b) } } filter { case (a,b) => boardContains(a, b) &&  (a,b) != (x,y) }
        //(for (xx <- moves; yy <- moves) yield (xx + x, yy + y)).filter { case (a,b) => ! ((a < 0) || (a > xmax) || (b < 0) || (b > ymax) || ((a ==
        //x && b == y))) } map
        neighborCoords map { case (aa,bb) => Node(aa,bb,board(aa)(bb)) }
      }
    }

    def nodeFor(x: Int, y: Int): Node = Node(x, y, board(x)(y))
    def boardContains(x: Int, y: Int): Boolean = (x >= 0 && x <= xmax && y >= 0 && y <= ymax)
  
    lazy val words = trav

    def trav(): List[String] = {

      def t00(ns: List[Node], path: List[Node], accum: List[String]): List[String] =      ns match {
        case Nil => accum
        case h::t => t00(t, path, t0(h, path, accum))
      }
      
      def t0(n: Node, path: List[Node], accum: List[String]): List[String] = {
        val currentPath = path :+ n
        val currentWord = currentPath.map(_.char)
        //only look at neighbors that we haven't already come across
        val filteredNeighbors = n.neighbors.filter(!path.contains(_))
        if (dict.prefix(currentWord)) {
          if (dict.full(currentWord)) { 
            currentWord.mkString("") :: t00(filteredNeighbors, currentPath, accum) 
          }
          else {
            t00(filteredNeighbors, currentPath, accum)
          }
        }
        else accum
      }

      (for (i <- 0 to xmax; j <- 0 to ymax) yield t0(nodeFor(i, j), List[Node](), List[String]())).flatten.toList
    }

    def containsWord(word: String): Boolean = words contains word
  }


  def generateRandomBoard(x: Int, y: Int): Board = {
    val r = new scala.util.Random
    val grid = 0 to x map {_ => 0 to y map { _ => (97 + r.nextInt(26)).toChar}}
    Board(grid)
  }


}
