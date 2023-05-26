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

package ai.dragonfly.math.matrix

import narr.*
import ai.dragonfly.math.vector.{Vec, *}
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.decomposition.{SV, *}

import scala.compiletime.ops.int.*
package object util {

  extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N]) {
    inline def asRowMatrix: Matrix[1, N] = Matrix[1, N](thisVector.asInstanceOf[NArray[Double]])
    inline def asColumnMatrix: Matrix[N, 1] = Matrix[N, 1](thisVector.asInstanceOf[NArray[Double]])
  }

  /**
   * Extension Methods for Square Matrices.
   */
  extension [MN <: Int] (m: Matrix[MN, MN])(using ValueOf[MN], ValueOf[Min[MN, MN]]) {
    /**
     * https://en.wikipedia.org/wiki/Invertible_matrix
     *
     * Computes the inverse of Square Matrix m.
     * @throws RuntimeException( "Matrix is singular." )
     * @return the inverse of matrix m
     */
    def inverse: Matrix[MN, MN] = solve(Matrix.identity[MN, MN])

    /** Solve a * x = b
     *
     * @param b right hand side
     * @return x = Matrix[MN, V] such that a * x = b
     */
    def solve[V <: Int](b: Matrix[MN, V])(using ValueOf[V]): Matrix[MN, V] = LU[MN, MN](m).solve(b)

    /** Matrix determinant
     * https://en.wikipedia.org/wiki/Determinant
     * the determinant is nonzero if and only if the matrix is invertible and the linear map represented by the matrix is an isomorphism
     * @return the determinant of this matrix.
     */
    def det(): Double = LU[MN, MN](m).det()

  }


  /**
   * Extension methods for rectangular matrices.
   */
  extension[M <: Int, N <: Int] (a: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], (N =:= M) =:= false) {
    /** Solve a * x = b
     *
     * @param b right hand side
     * @return least squares solution x = Matrix[M, V] such that a * x = b
     */
    def solve[V <: Int](b: Matrix[M, V])(using ValueOf[V]): Matrix[N, V] = QR[M, N](a).solve(b)
  }
  /**
   * Extension methods for rectangular matrices where M > N.
   */

  extension[M <: Int, N <: Int] (m: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], M > N =:= true) {
    /** Solve b * m = I[N, N]
     * m = Matrix[M, N] with M > N and Rank = N, has a left inverse b = Matrix[N, M] such that b * m = I[N, N]
     * @return b = Matrix[N, M] the Left Inverse of Matrix m.
     */
    def leftInverse(using ValueOf[Min[N, M]]): Matrix[N, M] = {
      val svd = SV[M, N, N](m)
      svd.V * svd.getS_Inverse() * svd.U.transpose
    }

  }

  /**
   * Extension methods for rectangular matrices where M < N.
   */

  extension[M <: Int, N <: Int] (m: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], N > M =:= true) {
    /**
     * m = Matrix[M, N] with M < N and Rank = M, has a right inverse b = Matrix[N, M] such that m * b = Identity[M, M]
     * @return the Right Inverse of Matrix a.
     */
    def rightInverse(using ValueOf[Min[M, M]]): Matrix[N, M] = QR[M, N](m).solve(Matrix.identity[M, M])

  }

}
