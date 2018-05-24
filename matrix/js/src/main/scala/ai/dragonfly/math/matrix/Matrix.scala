package ai.dragonfly.math.matrix

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportTopLevel("Jama.Matrix") @JSExportAll
object Matrix {

  /** Construct a matrix quickly without checking arguments.
    *
    * @param a Two-dimensional array of doubles.
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def apply (a: js.Array[js.Array[Double]], m: Int, n: Int): Jama.Matrix = new Jama.Matrix(a, m, n)

  /** Construct an m-by-n matrix of zeros.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def apply(m: Int, n: Int): Jama.Matrix = new Jama.Matrix(Array.fill[Array[Double]](m)(Array.fill[Double](n)(0.0)), m, n)

  /** Construct an m-by-n constant matrix.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @param s Fill the matrix with this scalar value.
    */
  def apply(m: Int, n: Int, s: Double): Jama.Matrix = new Jama.Matrix(Array.fill[Array[Double]](m)(Array.fill[Double](n)(s)), m, n)

  /** Construct a matrix from a 2-D array.
    *
    * @param a Two-dimensional array of doubles.
    * @exception IllegalArgumentException All rows must have the same length
    * @see #constructWithCopy
    */
  def apply(a: js.Array[js.Array[Double]]): Jama.Matrix = {
    println(s"$a | ${a.length} | ${a(0).length}")
    new Jama.Matrix(a, a.length, a(0).length)
  }

}
