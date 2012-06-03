package smallcheck

/**
 * Examples from the original SmallCheck
 */
object NumberProperties extends Properties("Number properties") {
  import Property._
  import Series.{seriesList}

  val primes: Stream[Int] = {
    def sieve(s: Stream[Int]): Stream[Int] = s match {
      case p #:: t =>
        p #:: sieve(for (x <- t if p*p > x || x % p > 0) yield x)
    }
    sieve(Stream.from(2))
  }
  
  val seriesNat = Series { (d: Int) =>
      for (i <- 0 to d) yield i
    }
  
  val seriesPrimes = Series { (d: Int) =>
      primes.take(d)
    }

  property("primes1") =
    forAll(seriesNat) { n =>
      n > 1 ==>
        forAll(seriesPrimes) { p =>
          p % n > 0 || p == n
        }
    }

  property("primes2") =
    forAll(seriesNat) { n =>
      n > 0 ==>
        thereExists1(seriesList(seriesNat)) { exps =>
          (exps.isEmpty || exps.last != 0) &&
          n == ((primes, exps).zipped map (scala.math.pow(_,_))).product.toInt
        }
    }
  
  def main(args: Array[String]): Unit = {
    Drivers.smallCheckI(NumberProperties)
  }
}
