
object Maybe {

  sealed trait Maybe[+A] {
    def map[B](f: A => B): Maybe[B]
    def flatMap[B](f: A => Maybe[B]): Maybe[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Maybe[B]): Maybe[B]
    def filter(f: A => Boolean): Maybe[A]
  }
  case class Some[+A](get: A) extends Maybe[A] {
    def map[B](f: A => B): Maybe[B] = Some(f(get))
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(get)
    def getOrElse[B >: A](default: => B): B = get
    def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = this
    def filter(f: A => Boolean): Maybe[A] = if (f(get)) this else None[A]
  }
  case class None[+A] extends Maybe[A] {
    def map[B](f: A => B): Maybe[B] = None[B]
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = None[B]
    def getOrElse[B >: A](default: => B): B = default
    def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = ob
    def filter(f: A => Boolean): Maybe[A] = this
  }

  def apply[A](v: A) = if (v==null) None() else Some(v)
  def map2[A,B,C](a: Maybe[A], b: Maybe[B], f: (A,B) => C): Maybe[C] = {
    for {
       aa <- a;
       bb <- b
    } yield f(aa,bb)
  }


  def sequence2[A](a: List[Maybe[A]]): Maybe[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
    }

  def traverse[A,B](as: List[A], f: A => Maybe[B]): Maybe[List[B]] = {
    as match {
      case Nil => Some(Nil)
      case a :: t => map2(f(a), traverse(t, f), (hh:B, tt:List[B]) => (hh::tt))
    }
  }

  def traverse2[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] = 
    as.foldRight(Some(List[B]()).asInstanceOf[Maybe[List[B]]]) { (a, accum) =>
      (f(a), accum) match {
        case (Some(b), Some(bs)) => Some(b :: bs)
        case _ => None()
      }
    }

}
