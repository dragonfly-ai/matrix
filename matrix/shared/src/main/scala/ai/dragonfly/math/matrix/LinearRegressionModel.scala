package ai.dragonfly.math.matrix


import ai.dragonfly.math.vector.Vector
import Jama.Matrix

import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Vector_Matrix
import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Matrix_Vector

case class LinearRegressionModel(center: Vector, beta: Matrix, error: Double) {
  def apply(x:Vector):Double = {
    val m: Matrix = x
    m.transpose().times(beta).get(0,0)
  }
}
