package Jama

import java.text.NumberFormat
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.io.PrintWriter

import ai.dragonfly.math.matrix.MatrixUtils._

import scala.scalajs.js.JSConverters._

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

object Matrix {
  /** Construct a matrix from a copy of a 2-D array.
    *
    * @param A Two-dimensional array of doubles.
    * @throws IllegalArgumentException All rows must have the same length
    */
  def constructWithCopy(A: Array[Array[Double]]): Matrix = {
    val a = new scala.scalajs.js.Array[scala.scalajs.js.Array[Double]](A.length)
    for (i <- a.indices) {
      val row = new scala.scalajs.js.Array[Double](A(i).length)
      for (j <- row.indices) {
        row(j) = A(i)(j)
      }
      a(i) = row
    }
    new Matrix(new js.Matrix(a))
  }

  /** Generate matrix with random elements
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @return An m-by-n matrix with uniformly distributed random elements.
    */
  def random(m: Int, n: Int): Matrix = {
    val a = new scala.scalajs.js.Array[scala.scalajs.js.Array[Double]](m)
    for (i <- a.indices) {
      val row = new scala.scalajs.js.Array[Double](n)
      for (j <- row.indices) {
        row(j) = Math.random()
      }
      a(i) = row
    }
    new Matrix(new js.Matrix(a))
  }

  /** Generate identity matrix
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @return An m-by-n matrix with ones on the diagonal and zeros elsewhere.
    */
  def identity(m: Int, n: Int): Matrix = {
    val a = new scala.scalajs.js.Array[scala.scalajs.js.Array[Double]](m)
    for (i <- a.indices) {
      val row = new scala.scalajs.js.Array[Double](n)
      for (j <- row.indices) {
        row(j) = if (i == j) 1 else 0
      }
      a(i) = row
    }
    new Matrix(new js.Matrix(a))
  }

}

class Matrix(mtrx: js.Matrix) extends Cloneable with Serializable {

  /** Construct an m-by-n matrix of zeros.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def this(m: Int, n: Int) {
    this(new js.Matrix(m, n))
  }

  /** Construct an m-by-n constant matrix.
    *
    * @param m Number of rows.
    * @param n Number of colums.
    * @param s Fill the matrix with this scalar value.
    */
  def this(m: Int, n: Int, s: Double) {
    this(new js.Matrix(m, n, s))
  }

  /** Construct a matrix from a 2-D array.
    *
    * @param A Two-dimensional array of doubles.
    * @throws IllegalArgumentException All rows must have the same length
    * @see #constructWithCopy
    */
  def this(A: Array[Array[Double]]) {
    this(new js.Matrix(A))
  }

  /** Construct a matrix quickly without checking arguments.
    *
    * @param A Two-dimensional array of doubles.
    * @param m Number of rows.
    * @param n Number of colums.
    */
  def this(A: Array[Array[Double]], m: Int, n: Int) {
    // This constructor was a bad idea.
    // Checking array.length is safer and incurs no meaningful performance penalties.
    // We don't use it.
    this(new js.Matrix(A))
  }

  /** Construct a matrix from a one-dimensional packed array
    *
    * @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
    * @param m    Number of rows.
    * @throws IllegalArgumentException Array length must be a multiple of m.
    */
  def this(vals: Array[Double], m: Int) {
    this(new js.Matrix(vals.toJSArray, m))
  }

  def jsMatrix(): js.Matrix = mtrx

  /** Make a deep copy of a matrix
    */
  def copy(): Matrix = {
    new Matrix(mtrx.copy)
  }

  /** Clone the Matrix object.
    */
  override def clone(): Object = this.copy

  /** Access the internal two-dimensional array.
    *
    * @return Pointer to the two-dimensional array of matrix elements.
    */
  def getArray(): Array[Array[Double]] = mtrx.getArray

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  def getArrayCopy(): Array[Array[Double]] = copy.getArray

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by columns.
    */
  def getColumnPackedCopy(): Array[Double] = mtrx.getColumnPackedCopy().toArray

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by rows.
    */
  def getRowPackedCopy(): Array[Double] = mtrx.getRowPackedCopy().toArray

  /** Get row dimension.
    *
    * @return m, the number of rows.
    */
  def getRowDimension(): Int = mtrx.getRowDimension()

  /** Get column dimension.
    *
    * @return n, the number of columns.
    */
  def getColumnDimension(): Int = mtrx.getColumnDimension()

  /** Get a single element.
    *
    * @param i Row index.
    * @param j Column index.
    * @return A(i,j)
    * @throws ArrayIndexOutOfBoundsException
    */
  def get(i: Int, j: Int): Double = mtrx.get(i, j)

  /** Get a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param j0 Initial column index
    * @param j1 Final column index
    * @return A(i0:i1,j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(i0: Int, i1: Int, j0: Int, j1: Int): Matrix = new Matrix(mtrx.getMatrix(i0, i1, j0, j1))

  /** Get a submatrix.
    *
    * @param r Array of row indices.
    * @param c Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r: Array[Int], c: Array[Int]): Matrix = new Matrix(
    mtrx.getMatrix(
      r.toJSArray,
      c.toJSArray
    )
  )

  /** Get a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param c  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(i0: Int, i1: Int, c: Array[Int]): Matrix = new Matrix(
    mtrx.getMatrix(i0, i1, c.toJSArray)
  )

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param j0 Initial column index
    * @param j1 Final column index
    * @return A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r: Array[Int], j0: Int, j1: Int): Matrix = new Matrix(
    mtrx.getMatrix(r.toJSArray, j0, j1)
  )

  /** Set a single element.
    *
    * @param i Row index.
    * @param j Column index.
    * @param s A(i,j).
    * @throws ArrayIndexOutOfBoundsException
    */
  def set(i: Int, j: Int, s: Double): Unit = mtrx.set(i, j, s)

  /** Set a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param j0 Initial column index
    * @param j1 Final column index
    * @param X  A(i0:i1,j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(i0: Int, i1: Int, j0: Int, j1: Int, X: Matrix): Unit = mtrx.setMatrix(
    i0, i1, j0, j1, X.jsMatrix
  )

  /** Set a submatrix.
    *
    * @param r Array of row indices.
    * @param c Array of column indices.
    * @param X A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r: Array[Int], c: Array[Int], X: Matrix): Unit = mtrx.setMatrix(
    r.toJSArray, c.toJSArray, X.jsMatrix
  )

  /** Set a submatrix.
    *
    * @param r  Array of row indices.
    * @param j0 Initial column index
    * @param j1 Final column index
    * @param X  A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r: Array[Int], j0: Int, j1: Int, X: Matrix): Unit = mtrx.setMatrix(
    r.toJSArray, j0, j1, X.jsMatrix
  )

  /** Set a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param c  Array of column indices.
    * @param X  A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(i0: Int, i1: Int, c: Array[Int], X: Matrix): Unit = mtrx.setMatrix(
    i0, i1, c.toJSArray, X.jsMatrix
  )

  /** Matrix transpose.
    *
    * @return A'
    */
  def transpose(): Matrix = new Matrix(mtrx.transpose())

  /** One norm
    *
    * @return maximum column sum.
    */
  def norm1(): Double = mtrx.norm1

  /** Two norm
    *
    * @return maximum singular value.
    */
  def norm2(): Double = mtrx.norm2

  /** Infinity norm
    *
    * @return maximum row sum.
    */
  def normInf(): Double = mtrx.normInf

  /** Frobenius norm
    *
    * @return sqrt of sum of squares of all elements.
    */
  def normF(): Double = mtrx.normF

  /** Unary minus
    *
    * @return -A
    */
  def uminus(): Matrix = new Matrix(mtrx.uminus)

  /** C = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plus(B: Matrix): Matrix = new Matrix(mtrx.plus(B.jsMatrix))

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plusEquals(B: Matrix): Matrix = new Matrix(mtrx.plusEquals(B.jsMatrix))

  /** C = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minus(B: Matrix): Matrix = new Matrix(mtrx.minus(B.jsMatrix))

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minusEquals(B: Matrix): Matrix = new Matrix(mtrx.minusEquals(B.jsMatrix))

  /** Element-by-element multiplication, C = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimes(B: Matrix): Matrix = new Matrix(mtrx.arrayTimes(B.jsMatrix))

  /** Element-by-element multiplication in place, A = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimesEquals(B: Matrix): Matrix = new Matrix(mtrx.arrayTimesEquals(B.jsMatrix))

  /** Element-by-element right division, C = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivide(B: Matrix): Matrix = new Matrix(mtrx.arrayRightDivide(B.jsMatrix))

  /** Element-by-element right division in place, A = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivideEquals(B: Matrix): Matrix = new Matrix(mtrx.arrayRightDivideEquals(B.jsMatrix))

  /** Element-by-element left division, C = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivide(B: Matrix): Matrix = new Matrix(mtrx.arrayLeftDivide(B.jsMatrix))

  /** Element-by-element left division in place, A = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivideEquals(B: Matrix): Matrix = new Matrix(mtrx.arrayLeftDivideEquals(B.jsMatrix))

  /** Multiply a matrix by a scalar, C = s*A
    *
    * @param s scalar
    * @return s*A
    */
  def times(s: Double): Matrix = new Matrix(mtrx.times(s))

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def timesEquals(s: Double): Matrix = {
    mtrx.timesEquals(s)
    this
  }

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param B another matrix
    * @return Matrix product, A * B
    * @throws IllegalArgumentException Matrix inner dimensions must agree.
    */
  def times(B: Matrix): Matrix = new Matrix(mtrx.times(B.jsMatrix))

  /** LU Decomposition
    *
    * @return LUDecomposition
    * @see LUDecomposition
    */
  def lu(): LUDecomposition = new LUDecomposition(this)

  /** QR Decomposition
    *
    * @return QRDecomposition
    * @see QRDecomposition
    */
  def qr(): QRDecomposition = new QRDecomposition(this)

  /** Cholesky Decomposition
    *
    * @return CholeskyDecomposition
    * @see CholeskyDecomposition
    */
  def chol(): CholeskyDecomposition = new CholeskyDecomposition(this)

  /** Singular Value Decomposition
    *
    * @return SingularValueDecomposition
    * @see SingularValueDecomposition
    */
  def svd(): SingularValueDecomposition = new SingularValueDecomposition(this)

  /** Eigenvalue Decomposition
    *
    * @return EigenvalueDecomposition
    * @see EigenvalueDecomposition
    */
  def eig(): EigenvalueDecomposition = new EigenvalueDecomposition(this)

  /** Solve A*X = B
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise
    */
  def solve(B: Matrix): Matrix = new Matrix(mtrx.solve(B.jsMatrix))

  /** Solve X*A = B, which is also A'*X' = B'
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise.
    */
  def solveTranspose(B: Matrix): Matrix = transpose.solve(B.transpose)

  /** Matrix inverse or pseudoinverse
    *
    * @return inverse(A) if A is square, pseudoinverse otherwise.
    */
  def inverse(): Matrix = new Matrix(mtrx.inverse)

  /** Matrix determinant
    *
    * @return determinant
    */
  def det(): Double = mtrx.det

  /** Matrix rank
    *
    * @return effective numerical rank, obtained from SVD.
    */
  def rank(): Int = mtrx.rank

  /** Matrix condition (2 norm)
    *
    * @return ratio of largest to smallest singular value.
    */
  def cond(): Double = mtrx.cond

  /** Matrix trace.
    *
    * @return sum of the diagonal elements.
    */
  def trace(): Double = mtrx.trace

  /** Print the matrix to stdout.   Line the elements up in columns
    *
    * with a Fortran-like 'Fw.d' style format.
    *
    * @param w Column width.
    * @param d Number of digits after the decimal.
    */
  def print(w: Int, d: Int): Unit = {
    print(new PrintWriter(System.out, true), w, d)
  }

  /** Print the matrix to the output stream.   Line the elements up in
    *
    * columns with a Fortran-like 'Fw.d' style format.
    *
    * @param output Output stream.
    * @param w      Column width.
    * @param d      Number of digits after the decimal.
    */
  def print(output: PrintWriter, w: Int, d: Int): Unit = {
    val format: DecimalFormat = new DecimalFormat
    format.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.US))
    format.setMinimumIntegerDigits(1)
    format.setMaximumFractionDigits(d)
    format.setMinimumFractionDigits(d)
    format.setGroupingUsed(false)
    print(output, format, w + 2)
  }

  /** Print the matrix to stdout.  Line the elements up in columns.
    * Use the format object, and right justify within columns of width
    * characters.
    * Note that is the matrix is to be read back in, you probably will want
    *
    * to use a NumberFormat that is set to US Locale.
    *
    * @param format A  Formatting object for individual elements.
    * @param width  Field width for each column.
    * @see java.text.DecimalFormat#setDecimalFormatSymbols
    */
  def print(format: NumberFormat, width: Int): Unit = {
    print(new PrintWriter(System.out, true), format, width)
  }

  /** Print the matrix to the output stream.  Line the elements up in columns.
    * Use the format object, and right justify within columns of width
    * characters.
    * Note that is the matrix is to be read back in, you probably will want
    *
    * to use a NumberFormat that is set to US Locale.
    *
    * @param output the output stream.
    * @param format A formatting object to format the matrix elements
    * @param width  Column width.
    * @see java.text.DecimalFormat#setDecimalFormatSymbols
    */
  def print(output: PrintWriter, format: NumberFormat, width: Int): Unit = {
    output.println() // start on new line.

    val m: Int = mtrx.getRowDimension()
    val n: Int = mtrx.getColumnDimension()

    output.println()  // start on new line.
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        val s = format.format(this.get(i, j)); // format the number
        val padding = Math.max(1,width-s.length()); // At _least_ 1 space
        for (k <- 0 until padding) output.print(' ')
        output.print(s)
      }
      output.println()
    }
    output.println()   // end with blank line.
  }

}
