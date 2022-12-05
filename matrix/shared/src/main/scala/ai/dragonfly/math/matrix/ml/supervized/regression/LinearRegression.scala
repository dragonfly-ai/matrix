package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.ml.data.{StaticSupervisedData, SupervisedData}
import ai.dragonfly.math.matrix.ml.supervized
import ai.dragonfly.math.matrix.ml.supervized.regression
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.vector.*

trait LinearRegression {

  def estimateBeta(X:Matrix, Y:Matrix): Matrix

  def train[V <: Vector](lrp:LinearRegressionProblem[V]): LinearRegressionModel[V] = {
    import lrp.*

    val A:Matrix = estimateBeta(X, Y)

    val errors:Matrix = X.times(A).minus(Y)

    var err:Double = 0.0
    var `err²`: Double = 0.0
    for (e <- errors.getRowPackedCopy()) {
      val et:Double = e*e
      err = err + Math.sqrt(et)
      `err²` = `err²` + et
    }

    regression.LinearRegressionModel[V](
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ))
    )
  }
}

trait LinearRegressionProblem[V <: Vector] {
  val X: Matrix
  val Y: Matrix
  val bias:Double
  val mean:V
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ
}

object LinearRegressionProblem {

  def apply[V <: Vector](trainingData:SupervisedData[V]):LinearRegressionProblem[V] = {
    new LinearRegressionProblem[V] {
      override val X: Matrix = trainingData.X
      override val Y: Matrix = trainingData.Y
      override val mean:V = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


