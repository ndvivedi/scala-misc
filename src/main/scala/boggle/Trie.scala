object Trie {
  import scala.collection._

  def apply[A <% Ordered[A]](items: List[List[A]]) = 
    items.foldRight(empty[A])((i:List[A],t:Trie[A]) => t.addItem(i))
  def empty[A <% Ordered[A]] = Trie(immutable.TreeMap[A, Trie[A]](), false)

  case class Trie[A <% Ordered[A]](next: immutable.TreeMap[A, Trie[A]], end: Boolean) {
    def addItem(item: List[A]): Trie[A] = item match {
      case Nil => Trie[A](next, true)
      case h::t => Trie[A](next + (h -> next.getOrElse(h, empty[A]).addItem(t)), end)
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

