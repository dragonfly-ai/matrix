package ai.dragonfly.math.matrix

import ai.dragonfly.math.matrix.decomposition.{Cholesky, Eigen, LU, QR, SV}
import ai.dragonfly.math.vector.*
import narr.*

import scala.math.hypot
import scala.scalajs.js


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


/*
  Because scala.js does not support static methods or fields, this object is not exported.
  However, the MatrixExports export of Matrix contains the native JS static methods for the Matrix class.
 */
object Matrix {
  /** Construct a matrix from a copy of a 2-D array.
   *
   * @param A Two-dimensional array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */
  def constructWithCopy(A: NArray[NArray[Double]]): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](A.length)(
      (row:Int) => NArray.tabulate[Double](A(0).length)(
        (col:Int) => A(row)(col)
      )
    )
  )

  /** Generate matrix with random elements
   *
   * @param m Number of rows.
   * @param n Number of colums.
   * @return An m-by-n matrix with uniformly distributed random elements.
   */
  def random(rows: Int, columns: Int, r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      _ => NArray.tabulate[Double](columns)(
        _ => r.nextDouble()
      )
    )
  )

  /** Generate identity matrix
   *
   * @param rows Number of rows.
   * @param columns Number of colums.
   * @return An m-by-n matrix with ones on the diagonal and zeros elsewhere.
   */
  def identity(rows: Int, columns: Int): Matrix = diagonal(rows, columns, 1.0)

  /** Generate identity matrix scaled by value parameter.
   *
   * @param rows Number of rows.
   * @param columns Number of colums.
   * @param value scalar multiplier.
   * @return An m-by-n matrix with ones on the diagonal and zeros elsewhere.
   */
  def diagonal(rows: Int, columns: Int, value:Double): Matrix = {
    val out:Matrix = new Matrix(rows, columns, 0.0)
    var i:Int = 0
    while (i < Math.min(rows, columns)) {
      out.set(i, i, value)
      i = i + 1
    }
    out
  }

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   * @param v a vector
   * @return
   */
  def diagonal(v:Vector): Matrix = {
    val out:Matrix = new Matrix(v.dimension, v.dimension, 0.0)
    var i:Int = 0
    while (i < v.dimension) {
      out.set(i, i, v.component(i))
      i = i + 1
    }
    out
  }


  /** Construct a matrix from a 2-D array.
   *
   * @param values Two-dimensional array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */

  def apply(values:NArray[NArray[Double]]):Matrix = new Matrix(values)
//    val l:Int = values(0).length
//    for (r <- 1 until values.length) {
//      if (values(r).length != l) throw new IllegalArgumentException("Cannot create a Matrix from a Jagged Array.")
//    }
//    new Matrix(values)
//  }

}

class Matrix  private(val values: NArray[NArray[Double]]) {

  inline def rows:Int = values.length
  inline def columns:Int = values(0).length

  /** Construct an m-by-n constant matrix.
   *
   * @param rows Number of rows.
   * @param columns Number of colums.
   * @param value Fill the matrix with this scalar value.
   */
  def this(rows: Int, columns: Int, value: Double) = this(
    NArray.tabulate[NArray[Double]](rows)(
      (_:Int) => NArray.fill[Double](columns)(value)
    )
  )

  /** Construct an m-by-n matrix of zeros.
   *
   * @param m Number of rows.
   * @param n Number of colums.
   */
  def this(m: Int, n: Int) = this(m, n, 0.0)


  /** Construct a matrix quickly without checking arguments.
   *
   * This constructor should not exist!
   *
   * @param A Two-dimensional array of doubles.
   * @param m Number of rows.
   * @param n Number of colums.
   */
  def this(values: NArray[NArray[Double]], m: Int, n: Int)  = this(values)

  /** Construct a matrix from a one-dimensional packed array
    *
    * @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
    * @param m    Number of rows.
    * @throws IllegalArgumentException Array length must be a multiple of m.
    */
  def this(vals: NArray[Double], m: Int) = this({
    val n:Int = vals.length / m
    if (m < 1 || m*n != vals.length) {
      throw new IllegalArgumentException(s"Matrix(vals:NArray[Double], m:Int) : m = $m does evenly divide vals.length = ${vals.length}.");
    }

    NArray.tabulate[NArray[Double]](m)(
      (i:Int) => NArray.tabulate[Double](n)(
        (j:Int) => vals(i + j*m)
      )
    )
  })

  /** Make a deep copy of a matrix
    */
  def copy(): Matrix = new Matrix(getArrayCopy())

  //  /** Clone the Matrix object.
  //    */
  //  override def clone: Any = js.native

  /** Access the internal two-dimensional array.
    *
    * @return Pointer to the two-dimensional array of matrix elements.
    */
  def getArray(): NArray[NArray[Double]] = values

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  def getArrayCopy(): NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](rows)(
    (row:Int) => NArray.tabulate[Double](columns)(
      (col:Int) => values(row)(col)
    )
  )

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by columns.
    */
  def getColumnPackedCopy(): NArray[Double] = {
    val vals: NArray[Double] = new NArray[Double](rows * columns)
    var i:Int = 0
    while (i < rows) {
      var j:Int = 0
      while (j < columns) {
        vals( i + j*rows) = values(i)(j)
        j = j + 1
      }
      i = i + 1
    }
    vals
  }

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by rows.
    */
  def getRowPackedCopy(): NArray[Double] = {
    val vals: NArray[Double] = new NArray[Double](rows * columns)
    var i: Int = 0
    while (i < rows) {
      var j: Int = 0
      while (j < columns) {
        vals( i*columns + j) = values(i)(j)
        j = j + 1
      }
      i = i + 1
    }
    vals
  }

  /** Get row dimension.
    *
    * @return m, the number of rows.
    */
  def getRowDimension(): Int = rows

  /** Get column dimension.
    *
    * @return n, the number of columns.
    */
  def getColumnDimension(): Int = columns

  /** Get a single element.
    *
    * @param i Row index.
    * @param j Column index.
    * @return A(i,j)
    * @throws ArrayIndexOutOfBoundsException
    */
  def get(r: Int, c: Int): Double = values(r)(c)

  /** Get a submatrix.
    *
    * @param i0 Initial row index
    * @param i1 Final row index
    * @param j0 Initial column index
    * @param j1 Final column index
    * @return A(i0:i1,j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r0: Int, r1: Int, c0: Int, c1: Int): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](r1 - r0 + 1)(
      (r:Int) => NArray.tabulate[Double](c1 - c0 + 1)(
        (c:Int) => values(r0 + r)(c0 + c)
      )
    )
  )

  /** Get a submatrix.
    *
    * @param r Array of row indices.
    * @param c Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(rowIndices: NArray[Int], columnIndices: NArray[Int]): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rowIndices.length)(
      (r:Int) => NArray.tabulate[Double](columnIndices.length)(
        (c:Int) => values(rowIndices(r))(columnIndices(c))
      )
    )
  )

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param r1 Final row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(r0: Int, r1: Int, columnIndices: NArray[Int]): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](r1 - r0 + 1)(
      (r:Int) => NArray.tabulate[Double](columnIndices.length)(
        (c:Int) => values(r + r0)(columnIndices(c))
      )
    )
  )

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @return A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix(rowIndices: NArray[Int], c0: Int, c1: Int): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rowIndices.length)(
      (r:Int) => NArray.tabulate[Double](c1 - c0 + 1)(
        (c:Int) => values(rowIndices(r))(c + c0)
      )
    )
  )

  /** Set a single element.
    *
    * @param r Row index.
    * @param c Column index.
    * @param value values(i,j).
    * @throws ArrayIndexOutOfBoundsException
    */
  inline def set(r: Int, c: Int, value: Double): Unit = values(r)(c) = value

  /** Set a submatrix.
    *
    * @param r0 Initial row index
    * @param r1 Final row index
    * @param c0 Initial column index
    * @param c1 Final column index
    * @param X  A(i0:i1,j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r0: Int, r1: Int, c0: Int, c1: Int, X: Matrix): Unit = {
    var r:Int = r0
    while (r <= r1) {
      var c = c0
      while (c <= c1) {
        values(r)(c) = X.get(r-r0,c-c0)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Set a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @param X A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(rowIndices: NArray[Int], columnIndices: NArray[Int], X: Matrix): Unit = {
    var i:Int = 0
    while (i < rowIndices.length) {
      var j:Int = 0
      while (j < columnIndices.length) {
        values(rowIndices(i))(columnIndices(j)) = X.get(i, j)
        j = j + 1
      }
      i = i + 1
    }
  }


  /** Set a submatrix.
    *
    * @param rowIndices  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @param X  A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(rowIndices: NArray[Int], c0: Int, c1: Int, X: Matrix): Unit = {
    var r:Int = 0
    while (r < rowIndices.length) {
      var c:Int = c0
      while (c <= c1) {
        values(rowIndices(r))(c) = X.get(r, c - c0)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Set a submatrix.
    *
    * @param r0 Initial row index
    * @param r1 Final row index
    * @param columnIndices  Array of column indices.
    * @param X  A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix(r0: Int, r1: Int, columnIndices: NArray[Int], X: Matrix): Unit = {
    var r:Int = r0
    while (r <= r1) {
      var c:Int = 0
      while (c < columnIndices.length) {
        values(r)(columnIndices(c)) = X.get(r - r0, c)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Matrix transpose.
    *
    * @return Mᵀ
    */
  def transpose(): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](columns)(
      (col:Int) => NArray.tabulate[Double](rows)(
        (row:Int) => values(row)(col)
      )
    )
  )

  /** One norm
    *
    * @return maximum column sum.
    */
  def norm1(): Double = {
    var maxColumnSum:Double = Double.MinValue
    var c:Int = 0
    while (c < columns) {
      var columnSum:Double = 0.0
      var r:Int = 0
      while (r < rows) {
        columnSum += Math.abs(values(r)(c))
        r = r + 1
      }
      maxColumnSum = Math.max(maxColumnSum, columnSum)
      c = c + 1
    }

    maxColumnSum
  }

  /** Two norm
    *
    * @return maximum singular value.
    */
  def norm2(): Double = SV(this).norm2()

  /** Infinity norm
    *
    * @return maximum row sum.
    */
  def normInf(): Double = {
    var maxRowSum:Double = Double.MinValue

    var r:Int = 0
    while (r < rows) {
      var rowSum:Double = 0.0
      var c:Int = 0
      while (c < columns) {
        rowSum += Math.abs(values(r)(c))
        c = c + 1
      }
      maxRowSum = Math.max(maxRowSum, rowSum)
      r = r + 1
    }

    maxRowSum
  }

  /** Frobenius norm
    *
    * @return sqrt of sum of squares of all elements.
    */
  def normF(): Double = {
    var f:Double = Double.MinValue

    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        f = hypot(f, values(r)(c))
        c = c + 1
      }
      r = r + 1
    }

    f
  }

  /** Unary minus
    *
    * @return -A
    */
  def uminus(): Matrix = times(- 1.0 )

  /** C = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plus(B: Matrix): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => values(r)(c) + B.get(r, c)
      )
    )
  )

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def plusEquals(B: Matrix): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) + B.get(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** C = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minus(B: Matrix): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => values(r)(c) - B.get(r, c)
      )
    )
  )

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def minusEquals(B: Matrix): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) - B.get(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Element-by-element multiplication, C = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimes(B: Matrix): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => values(r)(c) * B.get(r, c)
      )
    )
  )

  /** Element-by-element multiplication in place, A = A.*B
    *
    * @param B another matrix
    * @return A.*B
    */
  def arrayTimesEquals(B: Matrix): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) * B.get(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Element-by-element right division, C = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivide(B: Matrix): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => values(r)(c) / B.get(r, c)
      )
    )
  )

  /** Element-by-element right division in place, A = A./B
    *
    * @param B another matrix
    * @return A./B
    */
  def arrayRightDivideEquals(B: Matrix): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) / B.get(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }


  /** Element-by-element left division, C = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivide(B: Matrix): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => B.get(r, c) / values(r)(c)
      )
    )
  )

  /** Element-by-element left division in place, A = A.\B
    *
    * @param B another matrix
    * @return A.\B
    */
  def arrayLeftDivideEquals(B: Matrix): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = B.get(r, c) / values(r)(c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Multiply a matrix by a scalar, C = s*A
    *
    * @param s scalar
    * @return s*A
    */
  def times(s: Double): Matrix = new Matrix(
    NArray.tabulate[NArray[Double]](rows)(
      (r:Int) => NArray.tabulate[Double](columns)(
        (c:Int) => s * values(r)(c)
      )
    )
  )

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def timesEquals(s: Double): Matrix = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = s * values(r)(c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param B another matrix
    * @return Matrix product, A * B
    * @throws IllegalArgumentException Matrix inner dimensions must agree.
    */
  def times(B: Matrix): Matrix = {
    if (B.rows != columns) throw new IllegalArgumentException("Matrix inner dimensions must agree.")

    val X:Matrix = new Matrix(rows, B.columns)

    val Bcolj = new Array[Double](columns)

    var j:Int = 0
    while (j < B.columns) {
      var k:Int = 0
      while (k < columns) {
        Bcolj(k) = B.get(k, j)
        k = k + 1
      }
      var i:Int = 0
      while (i < rows) {
        val Arowi = values(i)
        var s:Double = 0.0
        k = 0
        while (k < columns) {
          s += Arowi(k) * Bcolj(k)
          k = k + 1
        }
        X.set(i, j, s)
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /** LU Decomposition
    *
    * @return LUDecomposition
    * @see LUDecomposition
    */
  def lu(): LU = LU(this)

  /** QR Decomposition
    *
    * @return QRDecomposition
    * @see QRDecomposition
    */
  def qr(): QR = QR(this)

  /** Cholesky Decomposition
    *
    * @return CholeskyDecomposition
    * @see CholeskyDecomposition
    */
  def chol(): Cholesky = Cholesky(this)

  /** Singular Value Decomposition
    *
    * @return SingularValueDecomposition
    * @see SingularValueDecomposition
    */
  def svd(): SV = SV(this)

  /** Eigenvalue Decomposition
    *
    * @return EigenvalueDecomposition
    * @see EigenvalueDecomposition
    */
  def eig(): Eigen = Eigen(this)

  /** Solve A*X = B
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise
    */
  def solve(B: Matrix): Matrix = {
    if (rows == columns) LU(this).solve(B)
    else QR(this).solve(B)
  }

  /** Solve X*A = B, which is also A'*X' = B'
    *
    * @param B right hand side
    * @return solution if A is square, least squares solution otherwise.
    */
  def solveTranspose(B: Matrix): Matrix = transpose().solve(B.transpose())

  /** Matrix inverse or pseudoinverse
    *
    * @return inverse(A) if A is square, pseudoinverse otherwise.
    */
  def inverse(): Matrix = solve(Matrix.identity(rows, rows))

  /** Matrix determinant
    *
    * @return determinant
    */
  def det(): Double = LU(this).det()

  /** Matrix rank
    *
    * @return effective numerical rank, obtained from SV.
    */
  def rank(): Int = SV(this).rank()

  /** Matrix condition (2 norm)
    *
    * @return ratio of largest to smallest singular value.
    */
  def cond(): Double = SV(this).cond()

  /** Matrix trace.
    *
    * @return sum of the diagonal elements.
    */
  def trace(): Double = {
    var t = 0.0
    var i:Int = 0
    while (i < Math.min(rows, columns)) {
      t += values(i)(i)
      i = i + 1
    }
    t
  }

}
