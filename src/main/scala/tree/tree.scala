object UBTree {

  sealed trait UBTree[A] {
    def contains(e: A)(implicit o: Ordering[A]): Boolean
    def insert(e: A)(implicit o: Ordering[A]): UBTree[A]
    def remove(e: A)(implicit o: Ordering[A]): UBTree[A]
  }
  case class Empty[A]() extends UBTree[A] {
    override def contains(e: A)(implicit o: Ordering[A]): Boolean = false
    override def insert(e: A)(implicit o: Ordering[A]): UBTree[A] = Node[A](e, Empty(), Empty())
    override def remove(e: A)(implicit o: Ordering[A]): UBTree[A] = this
  }
  case class Node[A](e: A, left: UBTree[A], right: UBTree[A]) extends UBTree[A] {
    override def contains(v: A)(implicit o: Ordering[A]): Boolean =  o.compare(e, v) match {
      case 0 => true
      case -1 => right.contains(v)
      case 1  => left.contains(v)
    }
    override def insert(v: A)(implicit o: Ordering[A]): UBTree[A] = o.compare(e, v) match {
      case 0 => this
      case -1 => Node[A](e, left, right.insert(v))
      case 1 => Node[A](e, left.insert(v), right)
    }
    override def remove(v: A)(implicit o: Ordering[A]): UBTree[A] = o.compare(e, v) match {
      case 0 => this match {
        case Node(_, _, Node(c, _, _)) => Node[A](c, left, right.remove(c))
        case Node(_, Node(c, _, _), Empty()) => Node[A](c, left.remove(c), right)
        case _ => Empty()
      }
      case -1 => Node[A](e, left, right.remove(v))
      case 1 => Node[A](e, left.remove(v), right)
    }
  }

  def traverse[A](t: UBTree[A]): Seq[A] = t match {
    case Empty() => List()
    case Node(x, l, r) => traverse(l) ++ List(x) ++ traverse(r)
  }
}
