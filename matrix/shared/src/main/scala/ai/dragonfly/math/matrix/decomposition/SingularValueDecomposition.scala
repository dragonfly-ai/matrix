package ai.dragonfly.math.matrix.decomposition

import ai.dragonfly.math.matrix.*
import bridge.array.*
import scala.math.hypot

object SingularValueDecomposition {

  def apply(M:Matrix):SingularValueDecomposition = {
    // Derived from LINPACK code.
    // Initialize.
    val A:ARRAY[ARRAY[Double]] = M.getArrayCopy()
    val rows:Int = M.getRowDimension()
    val columns:Int = M.getColumnDimension()

    /* Apparently the failing cases are only a proper subset of (rows<columns),
 so let's not throw error.  Correct fix to come later?
    if (rows<columns) {
  throw new IllegalArgumentException("Jama SVD only works for rows >= columns"); }
    */
    val nu = Math.min(rows, columns)
    val s:ARRAY[Double] = ARRAY.fill[Double](Math.min(rows + 1, columns))(0.0)
    val U:ARRAY[ARRAY[Double]] = ARRAY.tabulate[ARRAY[Double]](rows)(_ => ARRAY.fill[Double](nu)(0.0))
    val V:ARRAY[ARRAY[Double]] = ARRAY.tabulate[ARRAY[Double]](columns)(_ => ARRAY.fill[Double](columns)(0.0))
    val e:ARRAY[Double] = ARRAY.fill[Double](columns)(0.0)
    val work:ARRAY[Double] = ARRAY.fill[Double](rows)(0.0)

    // Reduce A to bidiagonal form, storing the diagonal elements
    // in s and the super-diagonal elements in e.

    val nct = Math.min(rows - 1, columns)
    val nrt = Math.max(0, Math.min(columns - 2, rows))

    for (k <- 0 until Math.max(nct, nrt)) {
      if (k < nct) { // Compute the transformation for the k-th column and
        // place the k-th diagonal in s[k].
        // Compute 2-norm of k-th column without under/overflow.
        s(k) = 0.0
        for (i <- k until rows) {
          s(k) = hypot(s(k), A(i)(k))
        }
        if (s(k) != 0.0) {
          if (A(k)(k) < 0.0) s(k) = -s(k)
          for (i <- k until rows) {
            A(i)(k) /= s(k)
          }
          A(k)(k) += 1.0
        }
        s(k) = -s(k)
      }
      for (j <- k + 1 until columns) {
        if ((k < nct) & (s(k) != 0.0)) { // Apply the transformation.
          var t = 0.0
          for (i <- k until rows) {
            t += A(i)(k) * A(i)(j)
          }
          t = -t / A(k)(k)
          for (i <- k until rows) {
            A(i)(j) += t * A(i)(k)
          }
        }
        // Place the k-th row of A into e for the
        // subsequent calculation of the row transformation.
        e(j) = A(k)(j)
      }
      if (k < nct) { // Place the transformation in U for subsequent back
        // multiplication.
        for (i <- k until rows) {
          U(i)(k) = A(i)(k)
        }
      }
      if (k < nrt) { // Compute the k-th row transformation and place the
        // k-th super-diagonal in e[k].
        // Compute 2-norm without under/overflow.
        e(k) = 0.0
        for (i <- k + 1 until columns) {
          e(k) = hypot(e(k), e(i))
        }
        if (e(k) != 0.0) {
          if (e(k + 1) < 0.0) e(k) = -e(k)
          for (i <- k + 1 until columns) {
            e(i) /= e(k)
          }
          e(k + 1) += 1.0
        }
        e(k) = -e(k)
        if ((k + 1 < rows) & (e(k) != 0.0)) {
          for (i <- k + 1 until rows) {
            work(i) = 0.0
          }
          for (j <- k + 1 until columns) {
            for (i <- k + 1 until rows) {
              work(i) += e(j) * A(i)(j)
            }
          }
          for (j <- k + 1 until columns) {
            val t = -e(j) / e(k + 1)
            for (i <- k + 1 until rows) {
              A(i)(j) += t * work(i)
            }
          }
        }
        // Place the transformation in V for subsequent
        // back multiplication.
        for (i <- k + 1 until columns) {
          V(i)(k) = e(i)
        }

      }
    }

    // Set up the final bidiagonal matrix or order p.

    var p = Math.min(columns, rows + 1)
    if (nct < columns) s(nct) = A(nct)(nct)
    if (rows < p) s(p - 1) = 0.0
    if (nrt + 1 < p) e(nrt) = A(nrt)(p - 1)
    e(p - 1) = 0.0

    // generate U.
    for (j <- nct until nu) {
      for (i <- 0 until rows) {
        U(i)(j) = 0.0
      }
      U(j)(j) = 1.0
    }
    for (k <- nct - 1 to 0 by -1) {
      if (s(k) != 0.0) {
        for (j <- k + 1 until nu) {
          var t = 0.0
          for (i <- k until rows) {
            t += U(i)(k) * U(i)(j)
          }
          t = -t / U(k)(k)
          for (i <- k until rows) {
            U(i)(j) += t * U(i)(k)
          }
        }
        for (i <- k until rows) {
          U(i)(k) = -U(i)(k)
        }
        U(k)(k) = 1.0 + U(k)(k)
        for (i <- 0 until k - 1) {
          U(i)(k) = 0.0
        }
      }
      else {
        for (i <- 0 until rows) {
          U(i)(k) = 0.0
        }
        U(k)(k) = 1.0
      }
    }

    // generate V.
    for (k <- columns - 1 to 0 by -1) {
      if ((k < nrt) & (e(k) != 0.0)) for (j <- k + 1 until nu) {
        var t = 0.0
        for (i <- k + 1 until columns) {
          t += V(i)(k) * V(i)(j)
        }
        t = -t / V(k + 1)(k)
        for (i <- k + 1 until columns) {
          V(i)(j) += t * V(i)(k)
        }
      }
      for (i <- 0 until columns) {
        V(i)(k) = 0.0
      }
      V(k)(k) = 1.0
    }

    // Main iteration loop for the singular values.

    val pp = p - 1
    var iter = 0
    val eps = Math.pow(2.0, -52.0)
    val tiny = Math.pow(2.0, -966.0)
    while ( p > 0 ) {

      // Here is where a test for too many iterations would go.
      // This section of the program inspects for
      // negligible elements in the s and e arrays.  On
      // completion the variables kase and k are set as follows.
      // kase = 1     if s(p) and e[k-1] are negligible and k<p
      // kase = 2     if s(k) is negligible and k<p
      // kase = 3     if e[k-1] is negligible, k<p, and
      //              s(k), ..., s(p) are not negligible (qr step).
      // kase = 4     if e(p-1) is negligible (convergence).

      var kase = 0
      var k = p - 2
      var continue:Boolean = true

      while ( k > -1 && continue ) {
        if (Math.abs(e(k)) <= tiny + eps * (Math.abs(s(k)) + Math.abs(s(k + 1)))) {
          e(k) = 0.0
          continue = false
        } else k -= 1
      }

      continue = true
      if (k == p - 2) kase = 4
      else {
        var ks = 0
        ks = p - 1
        while ( ks >= k && continue) {
          if (ks == k) {
            continue = false
          } else {
            val t = (if (ks != p) Math.abs(e(ks)) else 0.0) + (if (ks != k + 1) Math.abs(e(ks - 1)) else 0.0)
            if (Math.abs(s(ks)) <= tiny + eps * t) {
              s(ks) = 0.0
              continue = false
            } else ks -= 1
          }
        }
        if (ks == k) kase = 3
        else if (ks == p - 1) kase = 1
        else {
          kase = 2
          k = ks
        }
      }
      k += 1

      // Perform the task indicated by kase.

      kase match { // Deflate negligible s(p).
        case 1 =>
          var f = e(p - 2)
          e(p - 2) = 0.0
          for (j <- p - 2 to k by -1) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            if (j != k) {
              f = -sn * e(j - 1)
              e(j - 1) = cs * e(j - 1)
            }
            for (i <- 0 until columns) {
              t = cs * V(i)(j) + sn * V(i)(p - 1)
              V(i)(p - 1) = -sn * V(i)(j) + cs * V(i)(p - 1)
              V(i)(j) = t
            }
          }


        // Split at negligible s(k).
        case 2 =>
          var f = e(k - 1)
          e(k - 1) = 0.0
          for (j <- k until p) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            f = -sn * e(j)
            e(j) = cs * e(j)
            for (i <- 0 until rows) {
              t = cs * U(i)(j) + sn * U(i)(k - 1)
              U(i)(k - 1) = -sn * U(i)(j) + cs * U(i)(k - 1)
              U(i)(j) = t
            }
          }


        // Perform one qr step.
        case 3 =>
          // Calculate the shift.
          val scale = Math.max(Math.max(Math.max(Math.max(Math.abs(s(p - 1)), Math.abs(s(p - 2))), Math.abs(e(p - 2))), Math.abs(s(k))), Math.abs(e(k)))
          val sp = s(p - 1) / scale
          val spm1 = s(p - 2) / scale
          val epm1 = e(p - 2) / scale
          val sk = s(k) / scale
          val ek = e(k) / scale
          val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
          val c = (sp * epm1) * (sp * epm1)
          var shift = 0.0
          if ((b != 0.0) | (c != 0.0)) {
            shift = Math.sqrt(b * b + c)
            if (b < 0.0) shift = -shift
            shift = c / (b + shift)
          }
          var f = (sk + sp) * (sk - sp) + shift
          var g = sk * ek
          // Chase zeros.
          for (j <- k until p - 1) {
            var t = hypot(f, g)
            var cs = f / t
            var sn = g / t
            if (j != k) e(j - 1) = t
            f = cs * s(j) + sn * e(j)
            e(j) = cs * e(j) - sn * s(j)
            g = sn * s(j + 1)
            s(j + 1) = cs * s(j + 1)
            for (i <- 0 until columns) {
              t = cs * V(i)(j) + sn * V(i)(j + 1)
              V(i)(j + 1) = -sn * V(i)(j) + cs * V(i)(j + 1)
              V(i)(j) = t
            }
            t = hypot(f, g)
            cs = f / t
            sn = g / t
            s(j) = t
            f = cs * e(j) + sn * s(j + 1)
            s(j + 1) = -sn * e(j) + cs * s(j + 1)
            g = sn * e(j + 1)
            e(j + 1) = cs * e(j + 1)
            if (j < rows - 1) for (i <- 0 until rows) {
              t = cs * U(i)(j) + sn * U(i)(j + 1)
              U(i)(j + 1) = -sn * U(i)(j) + cs * U(i)(j + 1)
              U(i)(j) = t
            }
          }
          e(p - 2) = f
          iter = iter + 1


        // Convergence.
        case 4 =>
          // Make the singular values positive.
          if (s(k) <= 0.0) {
            s(k) = if (s(k) < 0.0) -(s(k))
            else 0.0
            for (i <- 0 to pp) {
              V(i)(k) = -V(i)(k)
            }
          }
          continue = true
          // Order the singular values.
          while ( k < pp & continue) {
            if (s(k) >= s(k + 1)) continue = false
            else {
              var t = s(k)
              s(k) = s(k + 1)
              s(k + 1) = t
              if (k < columns - 1) for (i <- 0 until columns) {
                t = V(i)(k + 1)
                V(i)(k + 1) = V(i)(k)
                V(i)(k) = t
              }
              if (k < rows - 1) for (i <- 0 until rows) {
                t = U(i)(k + 1)
                U(i)(k + 1) = U(i)(k)
                U(i)(k) = t
              }
              k += 1
            }
          }
          iter = 0
          p -= 1
      }
    }
    new SingularValueDecomposition(Matrix(U), Matrix(V), s, rows, columns)
  }
}

  /** Singular Value Decomposition.
  * <P>
  * For an rows-by-columns matrix A with rows >= columns, the singular value decomposition is
  * an rows-by-columns orthogonal matrix U, an columns-by-columns diagonal matrix S, and
  * an columns-by-columns orthogonal matrix V so that A = U*S*V'.
  * <P>n
  * The singular values, sigma[k] = S[k][k], are ordered so that
  * sigma[0] >= sigma[1] >= ... >= sigma[columns-1].
  * <P>
  * The singular value decompostion always exists, so the constructor will
  * never fail.  The matrix condition number and the effective numerical
  * rank can be computed from this decomposition.
  */

// Derived from LINPACK code.

/** Construct the singular value decomposition
 * Structure to access U, S and V.
 *
 * @param M Rectangular matrix
 */

class SingularValueDecomposition private (val U:Matrix, val V:Matrix, singularValues:ARRAY[Double], m:Int, n:Int){

  /** Return the left singular vectors
    *
    * @return U
    */
  def getU(): Matrix = U

  /** Return the right singular vectors
    *
    * @return V
    */
  def getV(): Matrix = V

  /** Return the one-dimensional array of singular values
    *
    * @return diagonal of S.
    */
  def getSingularValues():ARRAY[Double] = singularValues

  /** Return the diagonal matrix of singular values
    *
    * @return S
    */
  def getS(): Matrix = Matrix.diagonal(ai.dragonfly.math.vector.Vector(singularValues))

  /** Two norm
    *
    * @return max(S)
    */
  def norm2(): Double = singularValues(0)

  /** Two norm condition number
    *
    * @return max(S)/min(S)
    */
  def cond(): Double = singularValues(0) / singularValues(Math.min(m, n) - 1)

  /** Effective numerical matrix rank
    *
    * @return Number of nonnegligible singular values.
    */
  def rank(): Int = {
    val eps = Math.pow(2.0, -52.0)
    val tol = Math.max(m, n) * singularValues(0) * eps
    var r = 0
    for (i <- 0 until singularValues.length) {
      if (singularValues(i) > tol) r += 1
    }
    r
  }
}
