import fp.chp5._

def from(f: Int): Stream[Int] = Stream.unfold(f)(s => Some(s, s+1))
def constant(c:Int):Stream[Int] = Stream.unfold(c)(s => Some(s, s));
val fibs = Stream.unfold((0,1)){case (f0, f1) => Some(f0, (f1, f0+f1))}

constant(5).take(5).toList
from(5).take(3).toList
fibs.take(7).toList