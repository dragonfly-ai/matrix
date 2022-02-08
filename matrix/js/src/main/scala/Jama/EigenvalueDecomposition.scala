package Jama


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
  *
  * @param Arg Square matrix
  * @return Structure to access D and V.
  */

class EigenvalueDecomposition(val Arg: Matrix) extends Serializable {

  private val eigDec = new Jama.js.EigenvalueDecomposition(Arg.jsMatrix())

  /** Return the eigenvector matrix
    *
    * @return V
    */
  def getV(): Matrix = new Matrix(eigDec.getV())

  /** Return the real parts of the eigenvalues
    *
    * @return real(diag(D))
    */
  def getRealEigenvalues(): Array[Double] = eigDec.getRealEigenvalues().toArray

  /** Return the imaginary parts of the eigenvalues
    *
    * @return imag(diag(D))
    */
  def getImagEigenvalues(): Array[Double] = eigDec.getImagEigenvalues().toArray

  /** Return the block diagonal eigenvalue matrix
    *
    * @return D
    */
  def getD(): Matrix = new Matrix(eigDec.getD())
}
