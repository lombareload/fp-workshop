package fp.chp3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](ls: List[A]): List[A] = drop(ls, 1)

  def setHead[A](a: A, ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, rest) => Cons(a, rest)
  }

  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case Nil => Nil
    case Cons(_, tail) => if (n <= 0) ls else drop(tail, n - 1)
  }

  /*
  currying helps type inference otherwise dropWhile should be called
  with an explicit type argument as in dropWhile[Int](ls, p)
   */
  def dropWhile[A](ls: List[A])(p: A => Boolean): List[A] = ls match {
    case Cons(a, tail) => if (p(a)) dropWhile(tail)(p) else ls
    case Nil => Nil
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, tail) => Cons(a, init(tail))
  }

  def foldRight[A,B](ls: List[A], b: B)(f: (A, B) => B): B = ls match {
    case Cons(a, tail) => f(a, foldRight(tail, b)(f))
    case Nil => b
  }

  // exercise 3.10 implement tail recursive fold left
  def foldLeft[A,B](ls: List[A], b: B)(f: (B, A) => B): B = ls match {
    case Cons(a, tail) => foldLeft(tail, f(b, a))(f)
    case Nil => b
  }

  def product(ls: List[Int]): Int = {
    foldRight[Int, Int](ls, 1)(_*_)
  }

  def sum(ls: List[Int]): Int = {
    foldRight[Int, Int](ls, 0)(_+_)
  }

  def size[A](ls: List[A]): Int = {
    foldRight[A, Int](ls, 0)((_, n) => n+1)
  }

  def productL(ls: List[Int]): Int = {
    foldLeft[Int, Int](ls, 1)(_*_)
  }

  def sumL(ls: List[Int]): Int = {
    foldLeft[Int, Int](ls, 0)(_+_)
  }

  def sizeL[A](ls: List[A]): Int = {
    foldLeft[A, Int](ls, 0)((n, _) => n+1)
  }

  def reverse[A](ls: List[A]): List[A] = {
    foldLeft[A, List[A]](ls, Nil)((ls, a) => Cons(a,ls))
  }

  def append[A](l1: List[A], l2:List[A]): List[A] = {
    foldRight[A,List[A]](l1, l2)(Cons(_,_))
  }

  def concat[A](l1: List[List[A]]): List[A] = {
    foldLeft[List[A], List[A]](l1, Nil)(append)
  }

  def plusOne(ls: List[Int]): List[Int] = {
    foldRight(ls, Nil: List[Int])((i, ls) => Cons(i + 1, ls))
  }

  def toStr(ds: List[Double]): List[String] = {
    foldRight[Double, List[String]](ds, Nil)((d, ls) => Cons(d.toString,ls))
  }

  def map[A, B](ls: List[A])(f: (A) => B): List[B] = {
    foldRight[A, List[B]](ls, Nil)((a, bs) => Cons(f(a), bs))
  }

  def filter[A](ls: List[A])(p: A => Boolean): List[A] = {
    foldRight[A, List[A]](ls, Nil)((a, ls) => if(p(a)) Cons(a, ls) else ls)
  }

  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
    foldRight[A, List[B]](ls, Nil)((a, bs) => append(f(a), bs))
  }

  def filterF[A](ls: List[A])(p: A => Boolean): List[A] = {
    flatMap[A, A](ls)((a) => if (p(a)) Cons(a, Nil) else Nil)
  }

//  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
//    foldRight[Int, List[Int]](l1, l2)()
//  }
}
