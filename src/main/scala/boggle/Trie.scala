object Trie {
  import scala.collection._

  def apply[A](items: List[List[A]]) = 
    items.foldRight(empty[A])((i:List[A],t:Trie[A]) => t.addItem(i))
  def empty[A] = Trie(immutable.Map[A, Trie[A]](), false)

  case class Trie[A](next: immutable.Map[A, Trie[A]], end: Boolean) {
    def addItem(item: List[A]): Trie[A] = item match {
      case Nil => Trie[A](next, true)
      case h::t => Trie(next + (h -> next.getOrElse(h, empty).addItem(t)), end)
    }


    def full(item: List[A]): Boolean = 
      item match {
        case c::cs => next(c).full(cs)
        case Nil => end
      }

    def prefix(item: List[A]): Boolean =
      item match {
        case Nil => true
        case h::t => next(h).prefix(t)
      }
  }
}

