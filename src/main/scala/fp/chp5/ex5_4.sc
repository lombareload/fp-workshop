import fp.chp5._

Stream(2,4,6).forAll(_%2==0)
Stream(2,4,6,3,5).forAll(_%2==0)