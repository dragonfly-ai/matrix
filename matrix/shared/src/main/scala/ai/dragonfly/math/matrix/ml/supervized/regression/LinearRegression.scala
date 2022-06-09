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
import ai.dragonfly.math.visualization

trait LinearRegression {

  def estimateBeta(X:Matrix, Y:Matrix): Matrix

  def train(lrp:LinearRegressionProblem): LinearRegressionModel = {
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

    regression.LinearRegressionModel(
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ))
    )
  }
}

trait LinearRegressionProblem {
  val X: Matrix
  val Y: Matrix
  val bias:Double
  val mean:Vector
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ
}

object LinearRegressionProblem {

  def apply(trainingData:SupervisedData):LinearRegressionProblem = {
    new LinearRegressionProblem {
      override val X: Matrix = trainingData.X
      override val Y: Matrix = trainingData.Y
      override val mean:Vector = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


import ai.dragonfly.math.example.Demonstrable
