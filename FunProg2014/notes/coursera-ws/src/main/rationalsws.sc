val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.numer
x.denom

x - y - z
y + y
x < y
x max y
-x


import scala.annotation.tailrec

class Rational(x:Int, y:Int) {

  @tailrec
  private def gcd(a:Int, b:Int): Int =
    if(b == 0) a else gcd(b, a % b)

  private val g = gcd(x,y)

  def numer = x / g
  def denom = y / g

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this < that) that else this

  // Note space after the : as it is legal operator
  // Special Scala convention for prefix operator
  def unary_- :Rational = new Rational(-numer, denom)

  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def -(that: Rational) = this + -that
  override def toString = numer + "/" + denom

}