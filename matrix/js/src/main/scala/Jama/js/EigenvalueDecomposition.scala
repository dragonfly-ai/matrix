package Jama.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport


/** Eigenvalues and eigenvectors of a real matrix.
  * <P>
  * If A is symmetric, then A = V*D*V' where the eigenvalue matrix D is
  * diagonal and the eigenvector matrix V is orthogonal.
  *I.e. A = V.times(D.times(V.transpose())) and
  *V.times(V.transpose()) equals the identity matrix.
  * <P>
  * If A is not symmetric, then the eigenvalue matrix D is block diagonal
  * with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues,
  * lambda + i*mu, in 2-by-2 blocks, [lambda, mu; -mu, lambda].  The
  * columns of V represent the eigenvectors in the sense that A*V = V*D,
  *i.e. A.times(V) equals V.times(D).  The matrix V may be badly
  * conditioned, or even singular, so the validity of the equation
  * A = V*D*inverse(V) depends upon V.cond().
  * */

/** Check for symmetry, then construct the eigenvalue decomposition
  * Structure to access D and V.
  *
  * @param Arg Square matrix
  */
@JSImport("jama", "EigenvalueDecomposition") @js.native
class EigenvalueDecomposition(val Arg: Matrix) extends js.Object {

  /** Return the eigenvector matrix
    *
    * @return V
    */
  def getV: Matrix = js.native

  /** Return the real parts of the eigenvalues
    *
    * @return real(diag(D))
    */
  def getRealEigenvalues: js.Array[Double] = js.native

  /** Return the imaginary parts of the eigenvalues
    *
    * @return imag(diag(D))
    */
  def getImagEigenvalues: js.Array[Double] = js.native

  /** Return the block diagonal eigenvalue matrix
    *
    * @return D
    */
  def getD: Matrix = js.native
}
