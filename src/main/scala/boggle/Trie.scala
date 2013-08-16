object Trie {
  import scala.collection._

  def apply[A](items: List[List[A]]) = 
    items.foldLeft(empty[A])((t:Trie[A],i:List[A]) => t.addItem(i))
  def empty[A] = Trie(immutable.Map[A, Trie[A]](), false)

  case class Trie[A](next: immutable.Map[A, Trie[A]], end: Boolean) {
    def addItem(item: List[A]): Trie[A] = item match {
      case Nil => Trie[A](next, true)
      case h::t => Trie[A](next + (h -> next.getOrElse(h, empty).addItem(t)), end)
    }

    def full(item: List[A]): Boolean = 
      node(item).map {x => x.end} getOrElse false

    def prefix(item: List[A]): Boolean =
      node(item).map {x => true} getOrElse false

    def node(as: List[A]): Option[Trie[A]] = as match {
      case Nil => Some(this)
      case h::t => next.get(h).flatMap(_.node(t))
    }
  }
}

