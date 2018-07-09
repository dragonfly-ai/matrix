package Jama

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
  *
  * @param  Arg Square, symmetric matrix.
  * @return Structure to access L and isspd flag.
  */
class CholeskyDecomposition(val Arg: Matrix) extends Serializable { // Initialize.

  val cholDec = new js.CholeskyDecomposition(Arg.jsMatrix)

  def isSPD(): Boolean = cholDec.isSPD

  /** Return triangular factor.
    *
    * @return L
    */
  def getL() = new Matrix(cholDec.getL)

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is not symmetric positive definite.
    */
  def solve(B: Matrix): Matrix = new Matrix(cholDec.solve(B.jsMatrix))

}