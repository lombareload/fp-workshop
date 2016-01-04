import fp.chp3.{Leaf, Branch, Tree}

Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(6))))((x)=> x * x)