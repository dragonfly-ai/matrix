package Jama

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal


/**
  * Jama = Java Matrix class.
  * <P>
  * The Java Matrix Class provides the fundamental operations of numerical
  * linear algebra.  Various constructors create Matrices from two dimensional
  * arrays of double precision floating point numbers.  Various "gets" and
  * "sets" provide access to submatrices and matrix elements.  Several methods
  * implement basic matrix arithmetic, including matrix addition and
  * multiplication, matrix norms, and element-by-element array operations.
  * Methods for reading and printing matrices are also included.  All the
  * operations in this version of the Matrix Class involve real matrices.
  * Complex matrices may be handled in a future version.
  * <P>
  * Five fundamental matrix decompositions, which consist of pairs or triples
  * of matrices, permutation vectors, and the like, produce results in five
  * decomposition classes.  These decompositions are accessed by the Matrix
  * class to compute solutions of simultaneous linear equations, determinants,
  * inverses and other matrix functions.  The five decompositions are:
  * <P><UL>
  * <LI>Cholesky Decomposition of symmetric, positive definite matrices.
  * <LI>LU Decomposition of rectangular matrices.
  * <LI>QR Decomposition of rectangular matrices.
  * <LI>Singular Value Decomposition of rectangular matrices.
  * <LI>Eigenvalue Decomposition of both symmetric and nonsymmetric square matrices.
  * </UL>
  * <DL>
  * <DT><B>Example of use:</B></DT>
  * <P>
  * <DD>Solve a linear system A x = b and compute the residual norm, ||b - A x||.
  * <P><PRE>
  * double[][] vals = {{1.,2.,3},{4.,5.,6.},{7.,8.,10.}};
  * Matrix A = new Matrix(vals);
  * Matrix b = Matrix.random(3,1);
  * Matrix x = A.solve(b);
  * Matrix r = A.times(x).minus(b);
  * double rnorm = r.normInf();
  * </PRE></DD>
  * </DL>
  * *
  *
  * @author The MathWorks, Inc. and the National Institute of Standards and Technology.
  * @version 5 August 1998
  */
@js.native @JSGlobal
object Matrix extends js.Object { // with Cloneable {
  /** Construct a matrix from a copy of a 2-D array.
    *
    * @param A Two-dimensional array of doubles.
    * @exception IllegalArgumentException All rows must have the same length
    */
    def constructWithCopy(A: Array[Array[Double]]): Matrix = js.native

  /** Generate matrix with random elements
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @return An m-by-n matrix with uniformly distributed random elements.
    */
  def random(m: Int, n: Int): Matrix = js.native

  /** Generate identity matrix
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @return An m-by-n matrix with ones on the diagonal and zeros elsewhere.
    */
  def identity(m: Int, n: Int): Matrix = js.native
}

@js.native @JSGlobal
class Matrix extends js.Object {

  /** Construct an m-by-n matrix of zeros.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def this(m: Int, n: Int) = this()

  /** Construct an m-by-n constant matrix.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @param s Fill the matrix with this scalar value.
    */
  def this(m: Int, n: Int, s: Double) = this()

  /** Construct a matrix from a 2-D array.
    *
    * @param A Two-dimensional array of doubles.
    * @exception IllegalArgumentException All rows must have the same length
    * @see #constructWithCopy
    */
  def this(A: Array[Array[Double]]) = this()

  /** Construct a matrix quickly without checking arguments.
    *
    * @param A Two-dimensional array of doubles.
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def this(A: Array[Array[Double]], m: Int, n: Int)  = this()

  /** Construct a matrix from a one-dimensional packed array
    *
    * @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
    * @param m    Number of rows.
    * @exception IllegalArgumentException Array length must be a multiple of m.
    */
  def this(vals: Array[Double], m: Int) = this()

  /** Make a deep copy of a matrix
    */
  def copy: Matrix = js.native

//  /** Clone the Matrix object.
//    */
//  override def clone: Any = js.native

  /** Access the internal two-dimensional array.
    *
    * @return Pointer to the two-dimensional array of matrix elements.
    */
  def getArray: Array[Array[Double]] = js.native

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  def getArrayCopy: Array[Array[Double]] = js.native

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by columns.
    */
  def getColumnPackedCopy: Array[Double] = js.native

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by rows.
    */
  def getRowPackedCopy: Array[Double] = js.native

  /** Get row dimension.
    *
    * @return m, the number of rows.
    */
  def getRowDimension: Int = js.native

  /** Get column dimension.
    *
    * @return n, the number of columns.
    */
  def getColumnDimension: Int = js.native

  /** Get a single element.
    *
    * @param i Row index.
    * @param j Column index.
    * @return A(i,j)
    * @exception ArrayIndexOutOfBoundsException
    */
  def get(i: Int, j: Int): Double = js.native

  /** Get a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param j0 Initial column index
    * @param j1 Final column index
    * @return A(i0:i1,j0:j1)
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(i0: Int, i1: Int, j0: Int, j1: Int): Matrix = js.native

  /** Get a submatrix.
    *
    * @param r Array of row indices.
    * @param c Array of column indices.
    * @return A(r(:),c(:))
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r: Array[Int], c: Array[Int]): Matrix = js.native

  /** Get a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param c  Array of column indices.
    * @return A(i0:i1,c(:))
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(i0: Int, i1: Int, c: Array[Int]): Matrix = js.native

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param j0 Initial column index
    * @param j1 Final column index
    * @return A(r(:),j0:j1)
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r: Array[Int], j0: Int, j1: Int): Matrix = js.native

  /** Set a single element.
    *
    * @param i Row index.
    * @param j Column index.
    * @param s A(i,j).
    * @exception ArrayIndexOutOfBoundsException
    */
  def set(i: Int, j: Int, s: Double): Unit = js.native

  /** Set a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param j0 Initial column index
    * @param j1 Final column index
    * @param X  A(i0:i1,j0:j1)
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(i0: Int, i1: Int, j0: Int, j1: Int, X: Matrix): Unit = js.native

  /** Set a submatrix.
    *
    * @param r Array of row indices.
    * @param c Array of column indices.
    * @param X A(r(:),c(:))
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r: Array[Int], c: Array[Int], X: Matrix): Unit = js.native

  /** Set a submatrix.
    *
    * @param r  Array of row indices.
    * @param j0 Initial column index
    * @param j1 Final column index
    * @param X  A(r(:),j0:j1)
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r: Array[Int], j0: Int, j1: Int, X: Matrix): Unit = js.native

  /** Set a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param c  Array of column indices.
    * @param X  A(i0:i1,c(:))
    * @exception ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(i0: Int, i1: Int, c: Array[Int], X: Matrix): Unit = js.native

  /** Matrix transpose.
    *
    * @return A'
    */
  def transpose: Matrix = js.native

  /** One norm
    *
    * @return maximum column sum.
    */
  def norm1: Double = js.native

  /** Two norm
    *
    * @return maximum singular value.
    */
  def norm2: Double = js.native

  /** Infinity norm
    *
    * @return maximum row sum.
    */
  def normInf: Double = js.native

  /** Frobenius norm
    *
    * @return sqrt of sum of squares of all elements.
    */
  def normF: Double = js.native

  /** Unary minus
    *
    * @return -A
    */
  def uminus: Matrix = js.native

  /** C = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plus(B: Matrix): Matrix = js.native

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plusEquals(B: Matrix): Matrix = js.native

  /** C = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minus(B: Matrix): Matrix = js.native

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minusEquals(B: Matrix): Matrix = js.native

  /** Element-by-element multiplication, C = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimes(B: Matrix): Matrix = js.native

  /** Element-by-element multiplication in place, A = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimesEquals(B: Matrix): Matrix = js.native

  /** Element-by-element right division, C = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivide(B: Matrix): Matrix = js.native

  /** Element-by-element right division in place, A = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivideEquals(B: Matrix): Matrix = js.native

  /** Element-by-element left division, C = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivide(B: Matrix): Matrix = js.native

  /** Element-by-element left division in place, A = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivideEquals(B: Matrix): Matrix = js.native

  /** Multiply a matrix by a scalar, C = s*A
    *
    * @param s scalar
    * @return s*A
    */
  def times(s: Double): Matrix = js.native

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def timesEquals(s: Double): Matrix = js.native

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param B another matrix
    * @return Matrix product, A * B
    * @exception IllegalArgumentException Matrix inner dimensions must agree.
    */
  def times(B: Matrix): Matrix = js.native

  /** LU Decomposition
    *
    * @return LUDecomposition
    * @see LUDecomposition
    */
  def lu: LUDecomposition = js.native

  /** QR Decomposition
    *
    * @return QRDecomposition
    * @see QRDecomposition
    */
  def qr: QRDecomposition = js.native

  /** Cholesky Decomposition
    *
    * @return CholeskyDecomposition
    * @see CholeskyDecomposition
    */
  def chol: CholeskyDecomposition = js.native

  /** Singular Value Decomposition
    *
    * @return SingularValueDecomposition
    * @see SingularValueDecomposition
    */
  def svd: SingularValueDecomposition = js.native

  /** Eigenvalue Decomposition
    *
    * @return EigenvalueDecomposition
    * @see EigenvalueDecomposition
    */
  def eig: EigenvalueDecomposition = js.native

  /** Solve A*X = B
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise
    */
  def solve(B: Matrix): Matrix = js.native

  /** Solve X*A = B, which is also A'*X' = B'
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise.
    */
  def solveTranspose(B: Matrix): Matrix = js.native

  /** Matrix inverse or pseudoinverse
    *
    * @return inverse(A) if A is square, pseudoinverse otherwise.
    */
  def inverse: Matrix = js.native

  /** Matrix determinant
    *
    * @return determinant
    */
  def det: Double = js.native

  /** Matrix rank
    *
    * @return effective numerical rank, obtained from SVD.
    */
  def rank: Int = js.native

  /** Matrix condition (2 norm)
    *
    * @return ratio of largest to smallest singular value.
    */
  def cond: Double = js.native

  /** Matrix trace.
    *
    * @return sum of the diagonal elements.
    */
  def trace: Double = js.native

}
