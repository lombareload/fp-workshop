import fp.chp5._

Stream(1,2,3,4).map(_*2).headOption()
Stream(1,2,3,4,5,6).filter(_%2 ==0).toList
Stream(1,2,3).append(Stream(4,5,6)).toList
Stream(1,2,3).flatMap(i => Stream(i, i)).toList

def ones:Stream[Int] = Stream.cons(1, ones)

ones.map(_+1).exists(_%2==0)
ones.takeWhile(_==1).take(2).toList