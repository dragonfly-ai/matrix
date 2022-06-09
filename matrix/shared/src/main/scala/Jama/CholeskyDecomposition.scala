package Jama

import bridge.array.*

object CholeskyDecomposition {

  /** Cholesky Decomposition.
   * <P>
   * For a symmetric, positive definite matrix A, the Cholesky decomposition
   * is an lower triangular matrix L so that A = L*L'.
   * <P>
   * If the matrix is not symmetric or positive definite, the constructor
   * returns a partial decomposition and sets an internal flag that may
   * be queried by the isSPD() method.
   */
  /** Cholesky algorithm for symmetric and positive definite matrix.
   * Structure to access L and isspd flag.
   *
   * @param  mat Square, symmetric matrix.
   */

  def apply(mat:Matrix):CholeskyDecomposition = {
    // Initialize.
    val A = mat.getArrayCopy()
    val n = A.length
    val L = new Matrix(n, n).getArray()
    var isspd:Boolean = mat.columns == n
    // Main loop.
    for (j <- 0 until n) {
      val Lrowj = L(j)
      var d = 0.0
      for (k <- 0 until j) {
        val Lrowk = L(k)
        var s = 0.0
        for (i <- 0 until k) {
          s += Lrowk(i) * Lrowj(i)
        }
        s = (A(j)(k) - s) / L(k)(k)
        Lrowj(k) = s
        d = d + s * s
        isspd = isspd & (A(k)(j) == A(j)(k))
      }
      d = A(j)(j) - d
      isspd = isspd & (d > 0.0)
      L(j)(j) = Math.sqrt(Math.max(d, 0.0))
      for (k <- j + 1 until n) {
        L(j)(k) = 0.0
      }
    }
    new CholeskyDecomposition(L, isspd)
  }
}

class CholeskyDecomposition private (val larry:ARRAY[ARRAY[Double]], val isspd:Boolean) { // Initialize.

  inline def dimension:Int = larry.length

  /** Is the matrix symmetric and positive definite?
    *
    * @return true if A is symmetric and positive definite.
    */
  def isSPD(): Boolean = isspd

  /** Return triangular factor.
    *
    * @return L
    */
  def getL(): Matrix = Matrix(larry)

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is not symmetric positive definite.
    */
  def solve(B: Matrix): Matrix = {
    if (B.rows != dimension) throw new IllegalArgumentException("Matrix row dimensions must agree.")

    if (!isspd) {
      throw new RuntimeException("Matrix is not symmetric positive definite.")
    }
    // Copy right hand side.
    val X: ARRAY[ARRAY[Double]] = B.getArrayCopy()
    val nx: Int = B.columns
    // Solve L*Y = B;
    for (k <- 0 until dimension) {
      for (j <- 0 until nx) {
        for (i <- 0 until k) {
          X(k)(j) = X(k)(j) - X(i)(j) * larry(k)(i)
        }
        X(k)(j) = X(k)(j) / larry(k)(k)
      }
    }
    // Solve L'*X = Y;
    for (k <- dimension - 1 to 0 by -1) {
      for (j <- 0 until nx) {
        for (i <- k + 1 until dimension) {
          X(k)(j) = X(k)(j) - X(i)(j) * larry(i)(k)
        }
        X(k)(j) = X(k)(j) / larry(k)(k)
      }
    }
    Matrix(X)
  }

}

