package net.projecteuler.ediweissmann

/**
 * Function Memoization
 * Adapted from http://michid.wordpress.com/2009/02/23/function_mem/
 */

/**
 * Memoize a f(x, any) by ignoring second parameter value
 */
class Memoize2[-T, -U, +R](f: (T, U) => R) extends ((T, U) => R) {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T, a: U): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x, a)
      vals + ((x, y))
      y
    }
  }
}

object Memoize2 {
  def apply[T, U, R](f: (T, U) => R) = new Memoize2(f)
}
