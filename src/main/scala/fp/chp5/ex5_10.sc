import fp.chp5._

def fibs:Stream[Int] = {
  def loop(a:Int, b:Int): Stream[Int] = {
    val next = a + b
    Stream.cons(a, loop(b, b + a))
  }
  loop(0,1)
}

fibs.take(10).toList