// implement fibonacci with tail recursion
import scala.annotation.tailrec

def fib(nth: Int): Int = {
  @tailrec
  def loop(n: Int, prev: Int, acc: Int): Int = {
    if (n == 0) acc
    else loop(n - 1, acc, acc + prev)
  }
  loop(nth, 1, 0)
}

fib(0)

fib(1)

fib(2)

fib(3)

fib(4)

fib(5)
