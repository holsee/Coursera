
// PACKAGES

//commented out to get ws to run
//package foo
object bar {

}
class bam {

}
trait bump {

}

// import foo.bar
// import foo._  // wildcard
// import foo.{bar, bam, bump}


// TRAITS
// - Scala is Single Inheritance
// - but can leverage many traits
// - like interfaces in Java but more powerful
// - classes can have params, traits cannot
// - traits can provide default method implementations
// - ^ can be overridden

trait OhHai {
  val x: Int = 1
  def bish(z:Int) = z
}

trait OhEmGee {
  def bosh(y:Int) = y
}

class OhMerGerd(a:Int) {
  val numbor:Int = a
}

class OhNoes extends OhMerGerd(20) with OhHai with OhEmGee {
  override def bosh(y:Int) = x + y + numbor

  // Type nothing is like void / unit
  def error(msg: String):Nothing = throw new Error(msg)
}

val oh = new OhNoes
oh.bosh(10)
oh.bish(10)
oh.error("Uh Oh BuSketiOs!")
