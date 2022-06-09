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

