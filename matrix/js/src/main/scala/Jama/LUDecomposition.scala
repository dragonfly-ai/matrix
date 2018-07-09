package Jama

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
  *
  * @param  A Rectangular matrix
  * @return Structure to access L, U and piv.
  */
class LUDecomposition(val A: Matrix) extends Serializable { // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

  val luD = new Jama.js.LUDecomposition(A.jsMatrix)
  /** Is the matrix nonsingular?
    *
    * @return true if U, and hence A, is nonsingular.
    */
  def isNonsingular(): Boolean = luD.isNonsingular

  /** Return lower triangular factor
    *
    * @return L
    */
  def getL(): Matrix = new Matrix(luD.getL)

  /** Return upper triangular factor
    *
    * @return U
    */
  def getU(): Matrix = new Matrix(luD.getU)

  /** Return pivot permutation vector
    *
    * @return piv
    */
  def getPivot(): Array[Int] = luD.getPivot.toArray

  /** Return pivot permutation vector as a one-dimensional double array
    *
    * @return (double) piv
    */
  def getDoublePivot(): Array[Double] = luD.getDoublePivot.toArray

  /** Determinant
    *
    * @return det(A)
    * @throws IllegalArgumentException  Matrix must be square
    */
  def det(): Double = luD.det

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*U*X = B(piv,:)
    * @throws IllegalArgumentException Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is singular.
    */
  def solve(B: Matrix): Matrix = new Matrix(luD.solve(B.jsMatrix))
}
