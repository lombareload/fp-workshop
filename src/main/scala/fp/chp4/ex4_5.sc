import fp.chp4._

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case a::tail => f(a).flatMap(aa => traverse(tail)(f).map(b => aa::b))
  case Nil => Some(Nil)
}

def sequence[A](ls: List[Option[A]]): Option[List[A]] = {
  traverse(ls)(a => a)
}

sequence(List(Some(1), Some(2), Some(3), None))
sequence(List(Some(1), Some(2), Some(3)))