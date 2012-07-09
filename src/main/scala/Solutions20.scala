package net.projecteuler.ediweissmann

import math._

/*
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
 */
object P21 extends Solvable {

  def solve() = {

    def divisors(n: Int) = 1.until(sqrt(n.toDouble).toInt + 1).par
      .filter(n % _ == 0).map(i => Seq(i, n / i)).flatten.toSet - n // remove itself but keep 1

    // sum of divisors
    def d(n: Int): Int = divisors(n).sum

    // is amicable?
    def isAmicable(n: Int) = {
      val dn = d(n)
      dn != n && d(dn) == n
    }

    (1 to 10000).filter(isAmicable(_)).sum
  }

}

/*
n! means n x (n  1) x ... x 3 x 2 x 1

For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
 */
object P20 extends Solvable {

  def solve() = {

    def factorial(n:Int):BigInt = n match {
      case x if (x == 1) => 1
      case _ => n * factorial(n - 1)
    }

    factorial(100).toString.map(_.asDigit).sum
  }
}
