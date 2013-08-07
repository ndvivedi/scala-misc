object Ch5 {
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] = {
      uncons match {
        case None => Nil
        case Some((hd, tl)) => hd :: tl.toList
      }
    }

    def take(n: Int): Stream[A] = uncons match {
      case None => Stream.empty 
      case Some((hd, tl)) => n match {
        case 0 => Stream.empty
        case n => Stream.cons(hd, tl.take(n-1))
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
      case None => Stream.empty
      case Some((hd, tl)) => if (p(hd)) Stream.cons(hd, tl.takeWhile(p)) else Stream.empty
    }

    def forAll(p: A => Boolean): Boolean = uncons match {
      case None => true
      case Some((h, tl)) => p(h) && tl.forAll(p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

    def  

  }
  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }
    
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  } 
}
