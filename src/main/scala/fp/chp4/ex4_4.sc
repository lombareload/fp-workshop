import fp.chp4._

def sequence[A](ls: List[Option[A]]): Option[List[A]] = ls match {
  case opt::tail => opt.flatMap[List[A]](a => sequence(tail).map(aa => a::aa))
  case Nil => Some(Nil)
}

sequence(List(Some(1), Some(2), Some(3), None))
sequence(List(Some(1), Some(2), Some(3)))