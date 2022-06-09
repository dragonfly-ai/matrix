package ai.dragonfly.math.matrix

import Jama.*

import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import bridge.array.ARRAY

import scala.language.implicitConversions

// Based on: https://sthalles.github.io/svd-for-regression/

object LinearRegressionSVD extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {
    val svd:SingularValueDecomposition = X.svd()

    // Â = VS⁻ⁱUᵀ * Y
    svd.getV().times(svd.getS().inverse().times(svd.getU().transpose())).times(Y)

  }


}
