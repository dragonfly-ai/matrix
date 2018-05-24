package Jama

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport.Namespace
import scala.scalajs.js.annotation.{JSExportAll, JSGlobal, JSImport}

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
  * Structure to access U, S and V.
  *
  * @param Arg Rectangular matrix
  */
// Derived from LINPACK code.
@JSImport("jama", "SingularValueDecomposition")
@js.native
class SingularValueDecomposition(val Arg: Matrix) extends js.Object {

  /** Return the left singular vectors
    *
    * @return U
    */
  def getU: Matrix = js.native

  /** Return the right singular vectors
    *
    * @return V
    */
  def getV: Matrix = js.native

  /** Return the one-dimensional array of singular values
    *
    * @return diagonal of S.
    */
  def getSingularValues: js.Array[Double] = js.native

  /** Return the diagonal matrix of singular values
    *
    * @return S
    */
  def getS: Matrix = js.native

  /** Two norm
    *
    * @return max(S)
    */
  def norm2: Double = js.native

  /** Two norm condition number
    *
    * @return max(S)/min(S)
    */
  def cond: Double = js.native

  /** Effective numerical matrix rank
    *
    * @return Number of nonnegligible singular values.
    */
  def rank: Int = js.native
}
