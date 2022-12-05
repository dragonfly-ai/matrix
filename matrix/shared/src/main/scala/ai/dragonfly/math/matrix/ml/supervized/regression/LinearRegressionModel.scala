package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.util.*
import ai.dragonfly.math.vector.Vector
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

/**
 * @param A
 * @param standardError
 * @param `R²` Coefficient of determination =
 */


case class LinearRegressionModel[V <: Vector](A: Matrix, mean: V, bias: Double, standardError: Double, `R²`: Double) {
  val a: V = A.asVector.asInstanceOf[V]

  def apply(x: V): Double = (a dot (x - mean)) + bias
  //  def apply(X:Matrix):Matrix = {
  //    X.times(A)
  //  }

  import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

  override def toString(): String = s"LinearRegressionModel(\n\t\tA = ${A.dim},\n\t\tmean = $mean,\n\t\tbias = $bias,\n\t\tstandardError = $standardError,\n\t\tR² = ${`R²`}\n\t)"
}
