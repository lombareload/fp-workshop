import fp.chp5._

Stream[Int](1,2,3,4,5,6).take(4).toList
Stream(1,2,3,4,5).drop(3).toList