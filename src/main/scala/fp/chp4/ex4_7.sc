import fp.chp4._

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
  case Nil => Right(Nil)
  case a::tail => /*a.flatMap(aa => sequence(tail).map(aa::_))*/ for {
    aa <- a
    se <- sequence(tail)
  } yield aa::se
}

sequence[String, Int](List(Right(1), Right(2), Right(3)))
sequence[String, Int](List(Right(1), Left("wrong"), Right(2)))

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
  case Nil => Right(Nil)
  case a::tail => for {
    ea <- f(a)
    el <- traverse(tail)(f)
  } yield ea::el
}

traverse(List(2,4))(a => if (a % 2 == 0) Right(a) else Left(a))