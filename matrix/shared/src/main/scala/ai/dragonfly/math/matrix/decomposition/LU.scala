package ai.dragonfly.math.matrix.decomposition

import ai.dragonfly.math.matrix.*
import narr.*

object LU {

  def apply(A:Matrix):LU = {

    val LU:NArray[NArray[Double]] = A.getArrayCopy()
    val m:Int = A.getRowDimension()
    val n:Int = A.getColumnDimension()
    val piv:NArray[Int] = NArray.tabulate[Int](m)((i:Int) => i)

    var pivsign:Double = 1.0

    var LUrowi:NArray[Double] = null

    val LUcolj:NArray[Double] = NArray.fill[Double](m)(0.0)

    // Outer loop.

    var j:Int = 0; while (j < n) { // Make a copy of the j-th column to localize references.
      var i:Int = 0; while (i < m) {
        LUcolj(i) = LU(i)(j)
        i += 1
      }
      // Apply previous transformations.
      i = 0; while (i < m) {  // recycling i
        LUrowi = LU(i)
        // Most of the time is spent in the following dot product.
        val kmax = Math.min(i, j)
        var s = 0.0
        var k:Int = 0; while (k < kmax) {
          s += LUrowi(k) * LUcolj(k)
          k += 1
        }
        LUcolj(i) = LUcolj(i) - s
        LUrowi(j) = LUcolj(i)
        i += 1
      }
      // Find pivot and exchange if necessary.
      var p = j
      i = j + 1; while (i < m) {  // recycling i
        if (Math.abs(LUcolj(i)) > Math.abs(LUcolj(p))) p = i
        i += 1
      }
      if (p != j) {
        var k0:Int = 0; while (k0 < n) {
          val t = LU(p)(k0)
          LU(p)(k0) = LU(j)(k0)
          LU(j)(k0) = t
          k0 += 1
        }
        val k = piv(p)
        piv(p) = piv(j)
        piv(j) = k
        pivsign = -pivsign
      }
      // Compute multipliers.
      if (j < m & LU(j)(j) != 0.0) {
        var i0:Int = j + 1; while (i0 < m) {
          LU(i0)(j) = LU(i0)(j) / LU(j)(j)
          i0 += 1
        }
      }
      j += 1
    }
    new LU(LU, piv, pivsign)
  }

}

/** LU Decomposition.
  * <P>
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * <P>
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the constructor will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations.  This will fail if isNonsingular() returns false.
  */

/** LU Decomposition
  * Structure to access L, U and piv.
  *
  * @param  A Rectangular matrix
  */

class LU private(val LU:NArray[NArray[Double]], piv:NArray[Int], pivsign:Double) {  // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

  val m:Int = LU.length
  val n:Int = LU(0).length

  /** Is the matrix nonsingular?
    *
    * @return true if U, and hence A, is nonsingular.
    */
  def isNonsingular(): Boolean = {
    var j:Int = 0; while (j < n) {
      if (LU(j)(j) == 0.0) return false
      j += 1
    }
    true
  }

  /** Return lower triangular factor
    *
    * @return L
    */
  def getL(): Matrix = Matrix(
    NArray.tabulate[NArray[Double]](m)(
      (r:Int) => NArray.tabulate[Double](n)(
        (c:Int) => {
          if (r > c) LU(r)(c)
          else if (r == c) 1.0
          else 0.0
        }
      )
    )
  )


  /** Return upper triangular factor
    *
    * @return U
    */
  def getU(): Matrix = Matrix(
    NArray.tabulate[NArray[Double]](n)(
      (r:Int) => NArray.tabulate[Double](n)(
        (c:Int) => {
          if (r > c) 0.0
          else LU(r)(c)
        }
      )
    )
  )

  /** Return pivot permutation vector
    *
    * @return piv
    */
  def getPivot(): NArray[Int] = NArray.tabulate[Int](m)((i:Int) => piv(i))


  /** Return pivot permutation vector as a one-dimensional double array
    *
    * @return (double) piv
    */
  def getDoublePivot(): NArray[Double] = NArray.tabulate[Double](m)((i:Int) => piv(i).toDouble)

  /** Determinant
    *
    * @return det(A)
    * @throws IllegalArgumentException  Matrix must be square
    */
  def det(): Double = {
    if (m != n) throw new IllegalArgumentException("Matrix must be square.")

    var d:Double = pivsign

    var j:Int = 0; while (j < n) {
      d *= LU(j)(j)
      j += 1
    }

    d
  }

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*U*X = B(piv,:)
    * @throws IllegalArgumentException Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is singular.
    */
  def solve(B: Matrix): Matrix = {
    if (B.getRowDimension() != m) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!this.isNonsingular()) throw new RuntimeException("Matrix is singular.")

    // Copy right hand side with pivoting
    val nx:Int = B.getColumnDimension()
    val Xmat:Matrix = B.getMatrix(piv, 0, nx - 1)
    val X = Xmat.getArray()

    // Solve L*Y = B(piv,:)
    var k:Int = 0; while (k < n) {
      var i:Int = k + 1; while (i < n) {
        var j:Int = 0; while (j < nx) {
          X(i)(j) = X(i)(j) - (X(k)(j) * LU(i)(k))
          j += 1
        }
        i += 1
      }
      k += 1
    }
    // Solve U*X = Y;
    k = n - 1; while (k > -1) { // recycling k
      var j:Int = 0; while (j < nx) {
        X(k)(j) = X(k)(j) / LU(k)(k)
        j += 1
      }
      var i:Int = 0; while (i < k) {
        var j1:Int = 0; while (j1 < nx) {
          X(i)(j1) = X(i)(j1) - (X(k)(j1) * LU(i)(k))
          j1 += 1
        }
        i += 1
      }
      k -= 1
    }
    Xmat
  }
}
