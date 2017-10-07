package object popl18 {
  
  import Embedding.Predef._
  
  private var lvl = 0
  def test(name: String)(cde: => Unit): Unit = {
    val indent = "    " * lvl
    println(s"$indent> $name")
    lvl += 1
    //try { cde; println(s"${indent}OK.") }
    try cde
    catch { case e: Throwable => println(s"Failed ($e @ ${e.getStackTrace()(1)})") }
    finally lvl -= 1
  }
  
  def assertEqt(lhs: IR[_,_], rhs: IR[_,_]): Unit =
    if (lhs =~= rhs) () else {
      //System.err.println(s"Not equal. LHS:\n$lhs\nRHS:\n$rhs")
      throw new AssertionError(s"Not equal. LHS:\n$lhs\nRHS:\n$rhs")
    }
  
}
