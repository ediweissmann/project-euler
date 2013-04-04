package net.projecteuler.ediweissmann

import math._
import io.Source

object P23 extends Solvable {

  import Divisors.divisors

  def solve() = {
    def isAbundant(n:Int) = divisors(n).sum > n

    val abundants = (1 to 28123).par.filter(isAbundant(_)).toList

    def isSumOfAbundants(n:Int) = abundants.find(p => abundants.contains(n - p)).isDefined

    (1 to 28123).par.filter(isSumOfAbundants(_))
  }
}

/*
Using names.txt, a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938  53 = 49714.

What is the total of all the name scores in the file?
 */
object P22 extends Solvable {

  def solve() = {
    val names = Source.fromInputStream(getClass.getResourceAsStream("/names.txt")).mkString
      .trim.split(",").map(_.replace("\"", "")).sorted

    def worth(name: String) = name.map(_ - 'A' + 1).sum

    names.zipWithIndex.map({
      case (name, index) => worth(name) * (index + 1)
    }) sum
  }
}

/*
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
 */
object P21 extends Solvable {

  import Divisors.divisors

  def solve() = {

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

    def factorial(n: Int): BigInt = n match {
      case x if (x == 1) => 1
      case _ => n * factorial(n - 1)
    }

    factorial(100).toString.map(_.asDigit).sum
  }
}

object Divisors {

  def divisors(n: Int) = 1.until(sqrt(n.toDouble).toInt + 1).par
    .filter(n % _ == 0).map(i => Seq(i, n / i)).flatten.toSet - n // remove itself but keep 1
}
