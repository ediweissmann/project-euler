package net.projecteuler.ediweissmann

/**
 * Function Memoization
 * Adapted from http://michid.wordpress.com/2009/02/23/function_mem/
 */

/**
 * Memoize f(a, b)
 */
class Memoize2[-T, -U, +R](f: (T, U) => R) extends ((T, U) => R) {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[(T, U), R]

  def apply(a: T, b: U): R = {
    vals.getOrElseUpdate((a, b), {
      f(a, b)
    })
  }
}

object Memoize2 {

  def apply[T, U, R](f: (T, U) => R) = new Memoize2(f)

  def Y[T, U, R](f: ((T, U) => R) => (T, U) => R): ((T, U) => R) = {
    var yf: (T, U) => R = null
    yf = Memoize2(f(yf(_, _)))
    yf
  }
}
