package net.projecteuler.ediweissmann

trait Solvable {
  def solve(): Any

  override def toString: String = getClass.getSimpleName
}

object Main {

  def main(args: Array[String]) {
    val problem = args.toList match {
      case name :: tail => loadProblem(name)
      case _ => throw new RuntimeException("First argument should be problem number. Eg: P10")
    }

    println("Solution to %s is: %s".format(problem, problem.solve()))
  }

  /*
  * Load problem object by name
  */
  def loadProblem(name: String):Solvable = {
    val c = Class.forName("%s.%s$".format(getClass.getPackage.getName, name))
  c.getField("MODULE$").get(c).asInstanceOf[Solvable]
  }
}

object Conversions {
  // conversions from BigInt to BigInteger
  implicit def b2B(b: BigInt) = b.bigInteger
  implicit def B2b(b: java.math.BigInteger) = new BigInt(b)
}