package ai.dragonfly.math.matrix

import Jama.Matrix
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.test.{LinearRegressionTest, LinearRegressionTestScore}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}

trait LinearRegression {

  def estimateBeta(X:Matrix, Y:Matrix): Matrix

  def train(lrp:LinearRegressionProblem): LinearRegressionModel = {
    import lrp.*

    val beta:Matrix = estimateBeta(X, Y)

    val errors:Double = X.times(beta).minus(Y).norm2()

    LinearRegressionModel(beta, Math.sqrt(errors/size), 1.0 - (errors*errors / (`EstGaussian(Y)`.`σ̂²` * `EstGaussian(Y)`.ℕ̂)))
  }
}

trait LinearRegressionProblem {
  val X: Matrix
  val Y: Matrix
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ̂
}

object LinearRegressionProblem {

  def apply(trainingData:Array[LabeledVector]):LinearRegressionProblem = {
    val xDim = trainingData(0).vector.dimension

    val vX: MatrixValues = new MatrixValues(trainingData.size)
    val vY: MatrixValues = new MatrixValues(trainingData.size)
    var yGE = stream.Gaussian()

    var i = 0
    for (lv <- trainingData) {
      vX(i) = lv.vector.values //.copy().subtract(mean).values
      vY(i) = new VectorValues(1)
      vY(i)(0) = lv.label
      yGE.observe(1.0, lv.y)
      i = i + 1
    }

    new LinearRegressionProblem {
      override val X: Matrix = new Matrix(vX)
      override val Y: Matrix = new Matrix(vY)
      override val `EstGaussian(Y)`: EstimatedGaussian = yGE.estimate
    }
  }
}