package Jama

/** Singular Value Decomposition.
  * <P>
  * For an m-by-n matrix A with m >= n, the singular value decomposition is
  * an m-by-n orthogonal matrix U, an n-by-n diagonal matrix S, and
  * an n-by-n orthogonal matrix V so that A = U*S*V'.
  * <P>
  * The singular values, sigma[k] = S[k][k], are ordered so that
  * sigma[0] >= sigma[1] >= ... >= sigma[n-1].
  * <P>
  * The singular value decompostion always exists, so the constructor will
  * never fail.  The matrix condition number and the effective numerical
  * rank can be computed from this decomposition.
  */

/** Construct the singular value decomposition
  *
  * @param Arg Rectangular matrix
  * @return Structure to access U, S and V.
  */
class SingularValueDecomposition(val Arg: Matrix) extends Serializable { // Derived from LINPACK code.

  val svd = new Jama.js.SingularValueDecomposition(Arg.jsMatrix())

  /** Return the left singular vectors
    *
    * @return U
    */
  def getU(): Matrix = new Matrix(svd.getU())

  /** Return the right singular vectors
    *
    * @return V
    */
  def getV(): Matrix = new Matrix(svd.getV())

  /** Return the one-dimensional array of singular values
    *
    * @return diagonal of S.
    */
  def getSingularValues(): Array[Double] = svd.getSingularValues().toArray

  /** Return the diagonal matrix of singular values
    *
    * @return S
    */
  def getS(): Matrix = new Matrix(svd.getS())

  /** Two norm
    *
    * @return max(S)
    */
  def norm2(): Double = svd.norm2()

  /** Two norm condition number
    *
    * @return max(S)/min(S)
    */
  def cond(): Double = svd.cond()

  /** Effective numerical matrix rank
    *
    * @return Number of nonnegligible singular values.
    */
  def rank(): Int = svd.rank()
}
