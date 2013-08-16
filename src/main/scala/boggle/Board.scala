import scala.collection._

object Board {
  def apply(b: Vector[Vector[Char]]) = new Board(b)

  class Board(val board: Vector[Vector[Char]]) {
    val ymax = board(0).length - 1
    val xmax = board.length - 1

    case class Node(x: Int, y: Int, c: Char) {
      def char = board(x)(y)
      def neighbors(): List[Node] = {
        val moves = List(-1, 0, 1)
        (for (xx <- moves; yy <- moves) yield (xx + x, yy + y)).filter { case (a,b) => ! ((a < 0) || (a > xmax) || (b < 0) || (b > ymax) || ((a ==
        x && b == y))) } map
        { case (aa,bb) => Node(aa,bb,board(aa)(bb)) }
      }
    }

    def nodeFor(x: Int, y: Int): Node = Node(x, y, board(x)(y))
    //val words = wordsFrom(board)
    lazy val words = trav

    val dictWords = List("the", "his", "the", "to", "this", "fuck", "face")
    val dict = Trie(dictWords.map(_.toList))

    def trav(): List[String] = {

      def t00(ns: List[Node], path: List[Node], accum: List[String]): List[String] = {
        ns match {
          case Nil => accum
          case h::t => t00(t, path, t0(h, path, accum))
        }
      }
      def t0(n: Node, path: List[Node], accum: List[String]): List[String] = {
        val currentPath = path :+ n
        val currentWord = currentPath.map(_.char)
        if (dict.prefix(currentWord)) {
          if (dict.full(currentWord)) { 
            currentWord.mkString("") :: t00(n.neighbors, currentPath, accum) 
          }
          else {
            t00(n.neighbors, currentPath, accum)
          }
        }
        else accum
      }

      (for (i <- 0 to xmax; j <- 0 to ymax) yield t0(nodeFor(i, j), List[Node](), List[String]())).flatten.toList
      //t0(nodeFor(0, 0), List[Node](), List[String]())
    }

    def containsWord(word: String): Boolean = words contains word
  }


}
