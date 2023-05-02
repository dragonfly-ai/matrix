/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.matrix.decomposition

import ai.dragonfly.math.matrix.*
import narr.*

object Cholesky {

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
   * @param  mat Square, symmetric matrix.
   */

  def apply(mat:Matrix):Cholesky = {
    // Initialize.
    val A = mat.getArrayCopy()
    val n = A.length
    val L = new Matrix(n, n).getArray()
    var isspd:Boolean = mat.columns == n
    // Main loop.
    var j:Int = 0; while (j < n) {
      val Lrowj = L(j)
      var d = 0.0
      var k:Int = 0; while (k < j) {
        val Lrowk = L(k)
        var s = 0.0
        var i:Int = 0; while (i < k) {
          s += Lrowk(i) * Lrowj(i)
          i += 1
        }
        s = (A(j)(k) - s) / L(k)(k)
        Lrowj(k) = s
        d = d + s * s
        isspd = isspd & (A(k)(j) == A(j)(k))
        k += 1
      }
      d = A(j)(j) - d
      isspd = isspd & (d > 0.0)
      L(j)(j) = Math.sqrt(Math.max(d, 0.0))
      k = j + 1; while (k < n) { // recycling k
        L(j)(k) = 0.0
        k += 1
      }
      j += 1
    }
    new Cholesky(L, isspd)
  }
}

class Cholesky private(val larry:NArray[NArray[Double]], val isspd:Boolean) { // Initialize.

  inline def dimension:Int = larry.length

  /** Is the matrix symmetric and positive definite?
    *
    * @return true if A is symmetric and positive definite.
    */
  def isSPD(): Boolean = isspd

  /** Return triangular factor.
    *
    * @return L
    */
  def getL(): Matrix = Matrix(larry)

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is not symmetric positive definite.
    */
  def solve(B: Matrix): Matrix = {
    if (B.rows != dimension) throw new IllegalArgumentException("Matrix row dimensions must agree.")

    if (!isspd) {
      throw new RuntimeException("Matrix is not symmetric positive definite.")
    }
    // Copy right hand side.
    val X: NArray[NArray[Double]] = B.getArrayCopy()
    val nx: Int = B.columns
    // Solve L*Y = B;
    var k:Int = 0; while (k < dimension) {
      var j:Int = 0; while (j < nx) {
        var i:Int = 0; while (i < k) {
          X(k)(j) = X(k)(j) - X(i)(j) * larry(k)(i)
          i += 1
        }
        X(k)(j) = X(k)(j) / larry(k)(k)
        j += 1
      }
      k += 1
    }
    // Solve L'*X = Y;
    k = dimension - 1; while  (k > -1) { // recycling k
      var j:Int = 0; while (j < nx) {
        var i:Int = k + 1; while (i < dimension) {
          X(k)(j) = X(k)(j) - X(i)(j) * larry(i)(k)
          i += 1
        }
        X(k)(j) = X(k)(j) / larry(k)(k)
        j += 1
      }
      k -= 1
    }
    Matrix(X)
  }

}

