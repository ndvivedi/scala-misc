import scala.collection._

object Board {
  def apply(b: Vector[Vector[Char]]) = new Board(b)

  class Board(val board: Vector[Vector[Char]]) {

    //val words = wordsFrom(board)
    val words = Set("the", "he", "this", "his", "to")

    def containsWord(word: String): Boolean = words contains word
  }


  object Trie {

    case class Trie(next: mutable.Map[Char, Trie], wordEnd: Boolean = false) {
      def addWords(ws: List[String]): Trie = {
        val words = ws filter (_ != "")
        if (words.isEmpty) Trie(next, true)
        else {
          val wordsByFirst: List[(Char, List[String])] = words groupBy { _.head } mapValues {l => l.map(_.tail)} toList;

          val newNext = wordsByFirst.foldRight(next)((ww, m) => {
              m += (ww._1 -> m.getOrElse(ww._1, Trie(mutable.Map[Char, Trie]())).addWords(ww._2))
            }
          )
          Trie(newNext)
        }
      }

      def has(word: String): Boolean = {
        word.toList match {
          case c::cs => next(c).has(cs.mkString(""))
          case Nil => wordEnd
        }
      }
    }
  }
}
