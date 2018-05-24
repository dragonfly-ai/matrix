package Jama

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport.Namespace
import scala.scalajs.js.annotation.{JSExportAll, JSGlobal, JSImport}

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
  * @param  Arg Square, symmetric matrix.
  */
@JSImport("jama", "CholeskyDecomposition")
@js.native
class CholeskyDecomposition(val Arg: Matrix) extends js.Object { // Initialize.

  /** Is the matrix symmetric and positive definite?
    *
    * @return true if A is symmetric and positive definite.
    */
  def isSPD: Boolean = js.native

  /** Return triangular factor.
    *
    * @return L
    */
  def getL: Matrix = js.native

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @exception IllegalArgumentException  Matrix row dimensions must agree.
    * @exception RuntimeException  Matrix is not symmetric positive definite.
    */
  def solve(B: Matrix): Matrix = js.native
}

