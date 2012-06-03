package smallcheck

/**
 * Methods to invoke SmallCheck
 */
object Drivers {
  import Property.{TestCase, Pass, Fail, Inappropriate}

  /** Run SmallCheck on a property from depth 0 to a given depth d */
  def smallCheck(d: Int, p: Property)   = iterCheck(0, Some(d), p)

  /** Run SmallCheck on collection of properties from depth 0 to a given depth d */
  def smallCheck(d: Int, p: Properties) = iterCheck(0, Some(d), p)

  /** Run SmallCheck on a property at a given depth d */
  def depthCheck(d: Int, p: Property)   = iterCheck(d, Some(d), p)

  /** Run SmallCheck on collection of properties at a given depth d */
  def depthCheck(d: Int, p: Properties) = iterCheck(d, Some(d), p)

  /** Run SmallCheck on a property interactively */
  def smallCheckI(p: Property)   = iterCheck(0, None, p)

  /** Run SmallCheck on collection of properties */
  def smallCheckI(p: Properties) = iterCheck(0, None, p)

  private def iterCheck(dFrom: Int, dToOption: Option[Int], ps: Properties) {
    def iter(d: Int) {
      println("Depth "+d+":")
      var ok = true
      for ((name, p) <- ps.properties) {
        println(name)
        ok = ok & check(dToOption.isEmpty, 0, 0, true, p(d))
      }
      dToOption match {
        case None =>
          if (whenUserWishes("  Deeper")) iter(d+1)
        case Some(dTo) =>
          if (ok && d < dTo) iter(d+1)  
      }
    }
    iter(dFrom)
  }

  private def iterCheck(dFrom: Int, dToOption: Option[Int], p: Property) {
    def iter(d: Int) {
      println("Depth "+d+":")
      val ok = check(dToOption.isEmpty, 0, 0, true, p(d))
      dToOption match {
        case None =>
          if (whenUserWishes("  Deeper")) iter(d+1)
        case Some(dTo) =>
          if (ok && d < dTo) iter(d+1)
      }
    }
    iter(dFrom)
  }

  private def check(interactive: Boolean, numTests: Int, numIgnored: Int, ok: Boolean, rs: Seq[TestCase]): Boolean = {
    if (rs.isEmpty) {
      print("  Completed "+numTests+" test(s)")
      println(if (ok) " without failure." else ".")
      if (numIgnored > 0)
        println("  But "+numIgnored+" did not meet ==> condition.")
      ok
    } else {
      rs.head match {
        case TestCase(Inappropriate, _) =>
          check(interactive, numTests+1, numIgnored+1, ok, rs.tail)
        case TestCase(Pass, _) =>
          check(interactive, numTests+1, numIgnored, ok, rs.tail)
        case TestCase(Fail, args) =>
          println("  Failed test no. "+(numTests+1)+". Test values follow.")
          args foreach { arg => println("  " + arg) }
          if (interactive && whenUserWishes("  Continue"))
            check(interactive, numTests+1, numIgnored, false, rs.tail)
          else
            false
      }
    }
  }

  private def whenUserWishes(wish: String): Boolean = {
    print(wish+"? ")
    val reply = readLine()
    reply.isEmpty() || reply == "y"
  }
}
