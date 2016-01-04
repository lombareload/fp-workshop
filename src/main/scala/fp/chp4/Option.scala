package fp.chp4

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def filter(p: A => Boolean): Option[A] = this match {
    case Some(a) if(p(a)) => this
    case _ => None
  }

  def orElse[B >: A](opt: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case _ => opt
  }
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

