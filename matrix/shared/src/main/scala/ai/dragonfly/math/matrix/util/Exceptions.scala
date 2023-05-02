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

package ai.dragonfly.math.matrix.util

import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

enum MatrixOperation {
  case `+`, `-`, x, AxB, Ax, zip
}

case class MismatchedMatrixDimensions(m1:Matrix, m2:Matrix, op:MatrixOperation) extends Exception(
  s"Incompatible Matrix Dimensions under $op:\n ${m1.dim} and ${m2.dim}"
)

case class CannotExpressMatrixAsVector(m1:Matrix) extends Exception(
  s"To convert a Matrix to a Vector, one of its dimensions must be 1, but this matrix has dimensions: ${m1.dim}"
)


case class UnsupportedMatrixDimension(rows:Int, columns:Int) extends Exception(s"Can't create matrix with rows = $rows and columns = $columns.")

