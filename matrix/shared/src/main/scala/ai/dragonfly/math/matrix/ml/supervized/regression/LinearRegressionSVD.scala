package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.decomposition.SingularValueDecomposition

object LinearRegressionSVD extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {
    val svd: SingularValueDecomposition = X.svd()

    // Â = VS⁻ⁱUᵀ * Y
    svd.getV().times(svd.getS().inverse().times(svd.getU().transpose())).times(Y)

  }


}
