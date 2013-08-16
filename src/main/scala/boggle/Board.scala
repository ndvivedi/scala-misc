import scala.collection._

object Board {
  def apply(b: Vector[Vector[Char]]) = new Board(b)

  class Board(val board: Vector[Vector[Char]]) {

    //val words = wordsFrom(board)
    val words = Set("the", "he", "this", "his", "to")

    def containsWord(word: String): Boolean = words contains word
  }

}
