package Jama

import bridge.array.*

object QRDecomposition {

  def apply(M: Matrix): QRDecomposition = {
    // Initialize.
    val QR:ARRAY[ARRAY[Double]] = M.getArrayCopy()
    val rows:Int = M.getRowDimension()
    val columns:Int = M.getColumnDimension()
    val Rdiag:ARRAY[Double] = ARRAY.fill[Double](columns)(0.0)

    // Main loop.
    for (k <- 0 until columns) { // Compute 2-norm of k-th column without under/overflow.
      var nrm:Double = 0.0
      for (i <- k until rows) {
        nrm = util.Maths.hypot(nrm, QR(i)(k))
      }
      if (nrm != 0.0) { // Form k-th Householder vector.
        if (QR(k)(k) < 0) nrm = -nrm
        for (i <- k until rows) {
          QR(i)(k) /= nrm
        }
        QR(k)(k) += 1.0
        // Apply transformation to remaining columns.
        for (j <- k + 1 until columns) {
          var s = 0.0
          for (i <- k until rows) {
            s += QR(i)(k) * QR(i)(j)
          }
          s = -s / QR(k)(k)
          for (i <- k until rows) {
            QR(i)(j) += s * QR(i)(k)
          }
        }
      }
      Rdiag(k) = -nrm
    }
    new QRDecomposition(Matrix(QR), rows, columns, Rdiag)
  }

}

/** QR Decomposition.
  * <P>
  * For an rows-by-columns matrix A with rows >= columns, the QR decomposition is an rows-by-columns
  * orthogonal matrix Q and an columns-by-columns upper triangular matrix R so that
  * A = Q*R.
  * <P>
  * The QR decompostion always exists, even if the matrix does not have
  * full rank, so the constructor will never fail.  The primary use of the
  * QR decomposition is in the least squares solution of nonsquare systems
  * of simultaneous linear equations.  This will fail if isFullRank()
  * returns false.
  */
/** QR Decomposition, computed by Householder reflections.
  * Structure to access R and the Householder vectors and compute Q.
  *
  * @param A Rectangular matrix
  */

class QRDecomposition private (val QR: Matrix, val rows:Int, val columns:Int, val Rdiag: ARRAY[Double]) {

  /** Is the matrix full rank?
    *
    * @return true if R, and hence A, has full rank.
    */
  def isFullRank(): Boolean = {
    var i:Int = 0
    while (i < columns && Rdiag(i) != 0.0) i += 1
    i == columns
  }

  /** Return the Householder vectors
    *
    * @return Lower trapezoidal matrix whose columns define the reflections
    */
  def getH(): Matrix = Matrix(
    ARRAY.tabulate[ARRAY[Double]](rows)(
      (r:Int) => ARRAY.tabulate[Double](columns)(
        (c:Int) => if (r >= c) QR.get(r, c) else 0.0
      )
    )
  )

  /** Return the upper triangular factor
    *
    * @return R
    */
  def getR(): Matrix = Matrix(
    ARRAY.tabulate[ARRAY[Double]](rows)(
      (r:Int) => ARRAY.tabulate[Double](columns)(
        (c:Int) => {
          if (r < c) QR.get(r, c)
          else if (r == c) Rdiag(r)
          else 0.0
        }
      )
    )
  )

  /** Generate and return the (economy-sized) orthogonal factor
    *
    * @return Q
    */
  def getQ(): Matrix = {
    val X = new Matrix(rows, columns)
    val Q = X.getArray()
    for (k <- columns - 1 to 0 by -1) {
      for (i <- 0 until rows) {
        Q(i)(k) = 0.0
      }
      Q(k)(k) = 1.0
      for (j <- k until columns) {
        if (QR.get(k, k) != 0) {
          var s = 0.0
          for (i <- k until rows) {
            s += QR.get(i, k) * Q(i)(j)
          }
          s = -s / QR.get(k, k)
          for (i <- k until rows) {
            Q(i)(j) += s * QR.get(i, k)
          }
        }
      }
    }
    X
  }

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is rank deficient.
    */
  def solve(B: Matrix): Matrix = {
    if (B.getRowDimension() != rows) throw new IllegalArgumentException("Matrix row dimensions must agree.")
    if (!this.isFullRank()) throw new RuntimeException("Matrix is rank deficient.")

    // Copy right hand side
    val nx = B.getColumnDimension()
    val X = B.getArrayCopy()

    // Compute Y = transpose(Q)*B
    for (k <- 0 until columns) {
      for (j <- 0 until nx) {
        var s = 0.0
        for (i <- k until rows) {
          s += QR.get(i, k) * X(i)(j)
        }
        s = -s / QR.get(k, k)
        for (i <- k until rows) {
          X(i)(j) += s * QR.get(i, k)
        }
      }
    }
    // Solve R*X = Y;
    for (k <- columns - 1 to 0 by -1) {
      for (j <- 0 until nx) {
        X(k)(j) /= Rdiag(k)
      }
      for (i <- 0 until k) {
        for (j <- 0 until nx) {
          X(i)(j) -= X(k)(j) * QR.get(i, k)
        }
      }
    }
    Matrix(X).getMatrix(0, columns - 1, 0, nx - 1)
  }

}