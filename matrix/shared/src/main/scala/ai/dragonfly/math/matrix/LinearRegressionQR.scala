package ai.dragonfly.math.matrix

import Jama.{Matrix, QRDecomposition}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.stats.probability.distributions.stream.*
import ai.dragonfly.math.vector.*

import scala.language.implicitConversions

/*
 * Ported from: https://introcs.cs.princeton.edu/java/97data/MultipleLinearRegression.java.html
 * by Robert Sedgewick and Kevin Wayne.
*/

object LinearRegressionQR extends LinearRegression {

  override def train(trainingData:Array[LabeledVector]): LinearRegressionModel = {
    val xDim = trainingData(0).vector.dimension

    val yDist:EstimatedGaussian = {
      var eG = stream.Gaussian()
      trainingData.foreach(lv => {
        eG(1.0, lv.y)
      })
      eG.estimate
    }

    val vX: MatrixValues = new MatrixValues(trainingData.size)
    val vY: MatrixValues = new MatrixValues(trainingData.size)
    var i = 0
    for (lv <- trainingData) {
      vX(i) = lv.vector.values //.copy().subtract(mean).values
      vY(i) = new VectorValues(1)
      vY(i)(0) = lv.label
      i = i + 1
    }
    val X: Matrix = new Matrix(vX)
    val Y: Matrix = new Matrix(vY)

    val QRD: QRDecomposition = new QRDecomposition(X)
    val beta: Matrix = QRD.solve(Y)

    val errors:Double = X.times(beta).minus(Y).norm2()

    LinearRegressionModel(beta, Math.sqrt(errors/trainingData.length), 1.0 - (errors*errors / (yDist.σ̂ * yDist.ℕ̂)))
  }

}