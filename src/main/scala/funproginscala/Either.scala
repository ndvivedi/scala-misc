object Either2 {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case Right(v) => this
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        fst <- this;
        snd <- b
      } yield f(fst, snd)
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def traverse[A,E,B](as: List[A], f: A => Either[E,B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case a::aa => f(a).map2(traverse(aa, f))({(x,y) => x :: y})
    }
  }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(as, (x:Either[E,A]) => x)




}
