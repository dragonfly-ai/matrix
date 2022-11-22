package verification

import narr.NArray

import scala.Console.*

object Verification {

  def arrayCompare(a1:NArray[Double], a2:NArray[Double]):ArrayComparison = {

    if (a1.length == a2.length) {

      var discrepancies: Int = 0
      var combinedError: Double = 0.0
      var i: Int = 0;
      while (i < a1.length) {

        if (a1(i) != a2(i)) {
          discrepancies += 1
          var error: Double = Math.abs(a1(i) - a2(i))
          combinedError += error
        }

        i += 1
      }

      ArrayComparison(1, a1.length, discrepancies, combinedError)
    } else throw new Exception(s"${Console.RED}Dimensions do not match!${Console.RESET}")

  }

  def arrayCompare2D(a1:NArray[NArray[Double]], a2:NArray[NArray[Double]]):ArrayComparison = {

    if (a1.length == a2.length && a1(0).length == a2(0).length) {

      var discrepancies: Int = 0
      var r: Int = 0;
      var combinedError: Double = 0.0

      while (r < a1.length) {
        var c: Int = 0;
        while (c < a1(0).length) {
          val v0: Double = a1(r)(c)
          val v1: Double = a2(r)(c)

          if (v0 != v1) {
            var error: Double = Math.abs(v0 - v1)
            discrepancies += 1
            combinedError += error
          }
          c += 1
        }
        r += 1
      }

      ArrayComparison(a1.length, a1(0).length, discrepancies, combinedError)

    } else throw new Exception(s"${Console.RED}Dimensions do not match!${Console.RESET}")

  }

}

trait Verification {

  val mvs0: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](11)(
    (_: Int) => NArray.tabulate[Double](11)((_: Int) => Math.random())
  )

  val jm: Jama.Matrix = new Jama.Matrix(mvs0)
  val mm: ai.dragonfly.math.matrix.Matrix = ai.dragonfly.math.matrix.Matrix(mvs0)

  def name:String
  def run: Unit
  def verify: Unit = {
    println(s"Verifying ${Console.GREEN}$name${Console.RESET} against JAMA library.")
    run
    println(s"Verified ${Console.RED}$name${Console.RESET} against JAMA library.\n\n")
  }
}
