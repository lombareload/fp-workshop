import fp.chp3.List

List.flatMap(List(1,2,3,4))((i) => List(i, i*i))