// implement uncurry
def uncurry[A,B,C](fn: A => B => C): (A,B) => C = {
  (a, b) => fn(a)(b)
}