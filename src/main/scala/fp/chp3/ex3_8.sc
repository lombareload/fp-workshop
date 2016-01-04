import fp.chp3.List
import fp.chp3.Cons
import fp.chp3.Nil

List.foldRight[Int, List[Int]](List(1,2,3), Nil)(Cons(_,_))