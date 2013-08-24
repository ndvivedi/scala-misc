import Board._
import scala.collection._
object Game {
  type Player = String
  type Words = Set[String]

  class Game(val players: Set[String]) {

    val board = Board(Vector(Vector('t', 'h', 'e'), Vector('o', 'i', 's')))
    val leaderBoard = mutable.Map[Player, List[String]]() ++ players.map((_, List[String]())).toMap

    def accept(player: Player, word: String): Boolean = {
      val validWord = board containsWord word
      val playersWords = leaderBoard.getOrElse(player, List[String]())
      if (validWord) leaderBoard.put(player, word :: playersWords)
      validWord
    }

    def leaders(): Seq[(Player, Int)] = {
      val leadersWithScores = leaderBoard.toList.map(x => (x._1, x._2.map(_.length).sum))
      leadersWithScores sortBy (-_._2)
    }
  }

}
