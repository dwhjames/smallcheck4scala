SmallCheck for Scala
====================

SmallCheck for Scala is a Scala library for property-based testing. It
is a port of the original Haskell library,
[SmallCheck](http://www.cs.york.ac.uk/fp/smallcheck).

> Following the lead of **QuickCheck** (Claessen and Hughes 2000), …
> **SmallCheck** also [uses] type-based generators to obtain test-sets
> of finite values for which properties are checked, and report any
> counter-examples found. But instead of using a sample of randomly
> generated values they test properties for all values up to some
> limiting depth, progressively increasing this limit.

(Runciman et al. 2008).

The original SmallCheck is available on
[Hackage](http://hackage.haskell.org/package/smallcheck)
and is maintained by
[Roman Cheplyaka](http://github.com/feuerbach)
at
[http://github.com/feuerbach/smallcheck](http://github.com/feuerbach/smallcheck).

The original property-based testing library for Haskell,
[QuickCheck](http://hackage.haskell.org/package/QuickCheck)
(Claessen and Hughes 2000), has also spawned a port written in Scala:
[Rickard Nilsson](http://github.com/rickynils)’s
[ScalaCheck](https://github.com/rickynils/scalacheck). SmallCheck for Scala
has followed the design of ScalaCheck.

References
----------

Koen Claessen and John Hughes.
  QuickCheck: a lightweight tool for random testing of Haskell programs.
  In _Proceedings of the fifth ACM SIGPLAN international conference on Functional programming_,
  ICFP ’00, pages 268–279. ACM, 2000. doi:
  [10.1145/351240.351266](http://doi.acm.org/10.1145/351240.351266)

Colin Runciman, Matthew Naylor, and Fredrik Lindblad.
  SmallCheck and Lazy SmallCheck: automatic exhaustive testing for small values.
  In _Proceedings of the first ACM SIGPLAN symposium on Haskell_,
  Haskell ’08, pages 37–48. ACM, 2008. doi:
  [10.1145/1411286.1411292](http://doi.acm.org/10.1145/1411286.1411292)
