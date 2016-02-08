package fp.chp5
import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => lazy val head = h()
      head :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) cons(h(),t().take(n - 1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => val head = h()
      if (p(head)) cons[A](head, t().takeWhile(p)) else Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else empty)

  def headOption(): Option[A] = foldRight(None: Option[A])((a,b) => Some(a))

  def map[B](f:A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,s) => if(p(a)) cons(a, s) else s)

  def append[B >: A](t: Stream[B]): Stream[B] = foldRight(t)((a, tail) => cons(a, tail))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((b1,b2)=>f(b1).append(b2))

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def uMap[B](f:A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }

  def uTake(n: Int): Stream[A] = unfold(this,n){
    case (_, 0) => None
    case (Cons(h, t), n) => Some(h(),(t(), n-1))
  }

  def uTakeWhile(p: A => Boolean) = unfold(this){
    case Cons(h, t) => if (p(h())) Some(h(), t()) else None
  }

  def uZipWith[B,C](bs: Stream[B])(f:(A,B) => C): Stream[C] = unfold((this, bs): (Stream[A], Stream[B])){
    case (Cons(h, t), Cons(h1, t1)) => Some((f(h(),h1()), (t(), t1())))
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = uZipWith(s)(_==_).forAll(_==true)

  def tails():Stream[Stream[A]] = unfold(this)({
    case Cons(h, t) => Some((cons(h(),t()), t()))
    case _ => None
  })
}

case object Empty extends Stream[Nothing]

case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[B](b: B): Stream[B] = cons(b, constant(b))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A,S](z:S)(f: S => Option[(A, S)]): Stream[A] ={
    val optStream: Option[Stream[A]] = for{
      opt <- f(z)
      a <- Option(opt._1)
      s <- Option(opt._2)
    } yield cons(a, unfold(s)(f))
    optStream.getOrElse(empty[A])
  }
}
