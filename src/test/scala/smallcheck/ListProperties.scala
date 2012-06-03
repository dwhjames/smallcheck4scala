package smallcheck

/**
 * Examples from the original SmallCheck
 */
object ListProperties extends Properties("List properties") {
  import Property._
  
  property("rev-rev") =
    forAll { (xs: Stream[Int]) =>
      xs == xs.reverse.reverse
    }
  
  def isPrefix(xs: List[Int], ys: List[Int]): Boolean = (xs, ys) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (x::xs, y::ys) => x == y || isPrefix(xs, ys)
  }
  
  property("prefix-complete") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      isPrefix(xs, xs ++ ys)
    }
  
  property("prefix-sound") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      isPrefix(xs, ys) ==>
        exists { (zs: List[Int]) => ys == (xs ++ zs) }
    }
  
  property("union-1") =
    forAll { (xs: List[Boolean], ys: List[Boolean]) =>
      exists { (zs: List[Boolean]) =>
        forAll { (b: Boolean) =>
          zs.contains(b) == (xs.contains(b) || ys.contains(b))
        }
      }
    }
  
  property("union-2") =
    forAll { (xs: Stream[Boolean], ys: Stream[Boolean]) =>
      existsDeeperBy(_*2) { (zs: Stream[Boolean]) =>
        forAll { (b: Boolean) =>
          zs.contains(b) == (xs.contains(b) || ys.contains(b))
        }
      }
    }
  
  def main(args: Array[String]): Unit = {
    Drivers.smallCheckI(ListProperties)
  }
}
