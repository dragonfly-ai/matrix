package ai.dragonfly.math.matrix

import Jama.*

import ai.dragonfly.math.*
import vector.*
import matrix.*
import matrix.util.*
import demo.{LinearRegressionTest, LinearRegressionTestScore}

import scala.language.implicitConversions

/**
 * @param A
 * @param standardError
 * @param `R²` Coefficient of determination =
 */


case class LinearRegressionModel(A: Matrix, mean:Vector, bias:Double, standardError:Double, `R²`: Double) {
  val a:Vector = A.asVector
  def apply(x:Vector):Double = (a dot (x-mean)) + bias
//  def apply(X:Matrix):Matrix = {
//    X.times(A)
//  }
  import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

  override def toString(): String = s"LinearRegressionModel(\n\t\tA = ${A.dim},\n\t\tmean = $mean,\n\t\tbias = $bias,\n\t\tstandardError = $standardError,\n\t\tR² = ${`R²`}\n\t)"
}
