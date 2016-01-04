package fp.chp3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + depth(l).max(depth(r))
    case Leaf(_) => 1
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(x) => Leaf(f(x))
  }

  def fold[A,B](tree: Tree[A], b: B)(f:(A,B) => B): B = tree match {
    case Leaf(a) => f(a, b)
    case Branch(l, r) => fold(r, fold(l, b)(f))(f)
  }

//  def mapF[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
//    case Leaf(a) => fold[A,Tree[B]](tree, Leaf(f(a)))(x, t) => Tree(x)
//  }
}