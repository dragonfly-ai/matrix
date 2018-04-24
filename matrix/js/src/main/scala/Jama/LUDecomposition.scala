package Jama

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** LU Decomposition.
  * <P>
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * <P>
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the constructor will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations.  This will fail if isNonsingular() returns false.
  */

/** LU Decomposition
  * Structure to access L, U and piv.
  *
  * @param  A Rectangular matrix
  */

@js.native @JSGlobal
class LUDecomposition(val A: Matrix) extends js.Object {  // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

  /** Is the matrix nonsingular?
    *
    * @return true if U, and hence A, is nonsingular.
    */
  def isNonsingular: Boolean = js.native

  /** Return lower triangular factor
    *
    * @return L
    */
  def getL: Matrix = js.native

  /** Return upper triangular factor
    *
    * @return U
    */
  def getU: Matrix = js.native

  /** Return pivot permutation vector
    *
    * @return piv
    */
  def getPivot: Array[Int] = js.native

  /** Return pivot permutation vector as a one-dimensional double array
    *
    * @return (double) piv
    */
  def getDoublePivot: Array[Double] = js.native

  /** Determinant
    *
    * @return det(A)
    * @exception IllegalArgumentException  Matrix must be square
    */
  def det: Double = js.native

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*U*X = B(piv,:)
    * @exception IllegalArgumentException Matrix row dimensions must agree.
    * @exception RuntimeException  Matrix is singular.
    */
  def solve(B: Matrix): Matrix = js.native
}
