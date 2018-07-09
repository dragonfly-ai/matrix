package Jama

/** QR Decomposition.
  * <P>
  * For an m-by-n matrix A with m >= n, the QR decomposition is an m-by-n
  * orthogonal matrix Q and an n-by-n upper triangular matrix R so that
  * A = Q*R.
  * <P>
  * The QR decompostion always exists, even if the matrix does not have
  * full rank, so the constructor will never fail.  The primary use of the
  * QR decomposition is in the least squares solution of nonsquare systems
  * of simultaneous linear equations.  This will fail if isFullRank()
  * returns false.
  */

/** QR Decomposition, computed by Householder reflections.
  *
  * @param A Rectangular matrix
  * @return Structure to access R and the Householder vectors and compute Q.
  */
class QRDecomposition(val A: Matrix) extends Serializable { // Initialize.

  val qrDec = new Jama.js.QRDecomposition(A.jsMatrix)

  def isFullRank(): Boolean = qrDec.isFullRank

  /** Return the Householder vectors
    *
    * @return Lower trapezoidal matrix whose columns define the reflections
    */
  def getH(): Matrix = new Matrix(qrDec.getH)

  /** Return the upper triangular factor
    *
    * @return R
    */
  def getR(): Matrix = new Matrix(qrDec.getR)

  /** Generate and return the (economy-sized) orthogonal factor
    *
    * @return Q
    */
  def getQ(): Matrix = new Matrix(qrDec.getQ)

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is rank deficient.
    */
  def solve(B: Matrix): Matrix = new Matrix(qrDec.solve(B.jsMatrix))
}
