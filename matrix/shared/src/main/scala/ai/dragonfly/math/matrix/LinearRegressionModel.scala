package ai.dragonfly.math.matrix


import ai.dragonfly.math.vector.Vector
import Jama.Matrix
import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Vector_Matrix
import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Matrix_Vector
import ai.dragonfly.math.matrix.test.{LinearRegressionTest, LinearRegressionTestScore}

case class LinearRegressionModel(beta: Matrix, standardError:Double, `R²`: Double) {
  def apply(x:Vector):Double = {
    val m: Matrix = x
    m.transpose().times(beta).get(0,0)
  }
  override def toString(): String = s"LinearRegressionModel(beta = Matrix(${beta.getRowDimension()} x ${beta.getColumnDimension()}), standardError = $standardError, R² = ${`R²`})"
}
