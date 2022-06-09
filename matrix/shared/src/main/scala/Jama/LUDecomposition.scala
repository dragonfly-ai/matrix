package Jama

import bridge.array.*

object LUDecomposition {

  def apply(A:Matrix):LUDecomposition = {

    val LU:ARRAY[ARRAY[Double]] = A.getArrayCopy()
    val m:Int = A.getRowDimension()
    val n:Int = A.getColumnDimension()
    val piv:ARRAY[Int] = ARRAY.tabulate[Int](m)((i:Int) => i)

    var pivsign:Double = 1.0

    var LUrowi:ARRAY[Double] = null

    val LUcolj:ARRAY[Double] = ARRAY.fill[Double](m)(0.0)

    // Outer loop.

    for (j <- 0 until n) { // Make a copy of the j-th column to localize references.
      for (i <- 0 until m) {
        LUcolj(i) = LU(i)(j)
      }
      // Apply previous transformations.
      for (i <- 0 until m) {
        LUrowi = LU(i)
        // Most of the time is spent in the following dot product.
        val kmax = Math.min(i, j)
        var s = 0.0
        for (k <- 0 until kmax) {
          s += LUrowi(k) * LUcolj(k)
        }
        LUcolj(i) = LUcolj(i) - s
        LUrowi(j) = LUcolj(i)
      }
      // Find pivot and exchange if necessary.
      var p = j
      for (i <- j + 1 until m) {
        if (Math.abs(LUcolj(i)) > Math.abs(LUcolj(p))) p = i
      }
      if (p != j) {
        for (k <- 0 until n) {
          val t = LU(p)(k)
          LU(p)(k) = LU(j)(k)
          LU(j)(k) = t
        }
        val k = piv(p)
        piv(p) = piv(j)
        piv(j) = k
        pivsign = -pivsign
      }
      // Compute multipliers.
      if (j < m & LU(j)(j) != 0.0) for (i <- j + 1 until m) {
        LU(i)(j) = LU(i)(j) / LU(j)(j)
      }
    }
    new LUDecomposition(LU, piv, pivsign)
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

class LUDecomposition private (val LU:ARRAY[ARRAY[Double]], piv:ARRAY[Int], pivsign:Double) {  // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

  val m:Int = LU.length
  val n:Int = LU(0).length

  /** Is the matrix nonsingular?
    *
    * @return true if U, and hence A, is nonsingular.
    */
  def isNonsingular(): Boolean = {
    for (j <- 0 until n) {
      if (LU(j)(j) == 0.0) return false
    }
    true
  }

  /** Return lower triangular factor
    *
    * @return L
    */
  def getL(): Matrix = Matrix(
    ARRAY.tabulate[ARRAY[Double]](m)(
      (r:Int) => ARRAY.tabulate[Double](n)(
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
    ARRAY.tabulate[ARRAY[Double]](n)(
      (r:Int) => ARRAY.tabulate[Double](n)(
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
  def getPivot(): ARRAY[Int] = ARRAY.tabulate[Int](m)((i:Int) => piv(i))


  /** Return pivot permutation vector as a one-dimensional double array
    *
    * @return (double) piv
    */
  def getDoublePivot(): ARRAY[Double] = ARRAY.tabulate[Double](m)((i:Int) => piv(i).toDouble)

  /** Determinant
    *
    * @return det(A)
    * @throws IllegalArgumentException  Matrix must be square
    */
  def det(): Double = {
    if (m != n) throw new IllegalArgumentException("Matrix must be square.")

    var d:Double = pivsign

    for (j <- 0 until n) d *= LU(j)(j)

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
    for (k <- 0 until n) {
      for (i <- k + 1 until n) {
        for (j <- 0 until nx) {
          X(i)(j) = X(i)(j) - (X(k)(j) * LU(i)(k))
        }
      }
    }
    // Solve U*X = Y;
    for (k <- n - 1 to 0 by -1) {
      for (j <- 0 until nx) {
        X(k)(j) = X(k)(j) / LU(k)(k)
      }
      for (i <- 0 until k) {
        for (j <- 0 until nx) {
          X(i)(j) = X(i)(j) - (X(k)(j) * LU(i)(k))
        }
      }
    }
    Xmat
  }
}
