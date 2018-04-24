package Jama

import scala.scalajs.js


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
  * Structure to access R and the Householder vectors and compute Q.
  *
  * @param A Rectangular matrix
  */
@js.native
class QRDecomposition(val A: Matrix) extends js.Object {

  /** Is the matrix full rank?
    *
    * @return true if R, and hence A, has full rank.
    */
  def isFullRank: Boolean = js.native

  /** Return the Householder vectors
    *
    * @return Lower trapezoidal matrix whose columns define the reflections
    */
  def getH: Matrix = js.native

  /** Return the upper triangular factor
    *
    * @return R
    */
  def getR: Matrix = js.native

  /** Generate and return the (economy-sized) orthogonal factor
    *
    * @return Q
    */
  def getQ: Matrix = js.native

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @exception IllegalArgumentException  Matrix row dimensions must agree.
    * @exception RuntimeException  Matrix is rank deficient.
    */
  def solve(B: Matrix): Matrix = js.native

}