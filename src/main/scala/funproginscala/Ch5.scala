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
    def forAll2(p: A => Boolean): Boolean =
      foldRight(true)((a,b) => p(a) && b)

    def takeWhile2(p: A => Boolean): Stream[A] = {
      //i have no idea why i need to declare this val and use it
      //seems like its just to tell the compiler its a Stream[A] 
      //rather than a Stream[Nothing]
      // adding .asInstanceOf[Stream[A]] does the trick
      //val e: Stream[A] = Stream.empty
      foldRight(Stream.empty.asInstanceOf[Stream[A]])(
        (a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)
    }

    def map[B](f: A => B): Stream[B] = 
      foldRight(Stream.empty.asInstanceOf[Stream[B]])( 
        (a,b) => Stream.cons(f(a), b))
    def filter(p: A => Boolean): Stream[A] = 
      foldRight(Stream.empty.asInstanceOf[Stream[A]])(
        (a,b) => if (p(a)) Stream.cons(a, b) else b)
    def append[B >: A](other: Stream[B]): Stream[B] = 
      foldRight(other)((a,b) => Stream.cons(a,b))
    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream.empty.asInstanceOf[Stream[B]])(
        (a,b) => f(a).append(b))
    }

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

  def constant[A](v: A): Stream[A] = 
    Stream.cons(v, constant(v))

  def from(n: Int): Stream[Int] = 
    Stream.cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def fibs0(a:Int, b:Int): Stream[Int] = 
      Stream.cons(a, fibs0(b, b+a))
    fibs0(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def constant2[A](v: A): Stream[A] = 
    unfold(v)(s => Some((v, v)))
  def ones2: Stream[Int] = unfold(1)(x=> Some(x,x))

  def from2(n: Int): Stream[Int] = 
    unfold(n)(s => Some((n, n+1)))

  def fibs2: Stream[Int] =
    //ugly, any way to do a small/simple match against the tuple?
    //unfold((0,1))(s => Some((s._1, (s._2, s._2 + s._1))))
    unfold((0,1)){case (f,s) => Some((f, (s, f+s)))}
  
  def map2[A,B](as: Stream[A], f: A => B): Stream[B] = {
    unfold(as)((v: Stream[A]) => v.uncons.map(x => (f(x._1), x._2)))
  }

  def take2[A](as: Stream[A], n: Int): Stream[A] = 
    unfold((as, n))((s: (Stream[A], Int)) => 
        s match {
          case (as, 0) => None
          case (as, n) => as.uncons.map(x => (x._1, (x._2, n - 1)))
        })

  def takeWhile[A](as: Stream[A], p: A => Boolean): Stream[A] = 
    unfold(as)(st => 
      st.uncons.flatMap{case (h,t) => if (p(h)) Some((h, t)) else None})

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B], f: (A,B) => C): Stream[C] = 
    unfold((as, bs)){ case (aa, bb) =>
      (aa.uncons, bb.uncons) match {
        case (Some((ah, at)), Some((bh, bt))) => Some((f(ah, bh)), (at, bt))
        case _ => None
      }
    }

  //cheating since i didnt use previously defined methods i guess.
  def startsWith[A](as: Stream[A], bs: Stream[A]): Boolean = {
    (as uncons, bs uncons) match {
      case(None, None) => true
      case(Some(_), None) => true
      case(Some((ah, at)), Some((bh, bt))) => 
        if (ah == bh) startsWith(at, bt) else false
    }
  }
  def startsWith2[A](a1: Stream[A], a2: Stream[A]): Boolean = {
    zipWith(a1, a2, (a: A, b: A) => (a, b)).forAll(x => x._1 == x._2)
  }
}
