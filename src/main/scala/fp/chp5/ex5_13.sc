import fp.chp5._

Stream(1,2,3).uMap(_+1).toList
Stream.from(1).uTake(5).toList
Stream.from(1).uTakeWhile(_<10).toList
Stream(1,2,3).uZipWith(Stream(6,5,4,3))(_+_).toList
Stream(1,2,3).startsWith(Stream(1,2,3))
Stream(1,2,3).tails().map(s => s.toList).toList