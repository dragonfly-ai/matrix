package ai.dragonfly.math.matrix

import Jama.{Matrix, QRDecomposition}
import ai.dragonfly.math.matrix.test.{LinearRegressionTest, LinearRegressionTestScore}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.stream.*
import ai.dragonfly.math.vector.*

import scala.language.implicitConversions

/*
 * Ported from: https://introcs.cs.princeton.edu/java/97data/MultipleLinearRegression.java.html
 * by Robert Sedgewick and Kevin Wayne.
*/

object LinearRegressionQR {

  def train(labeledPoints:LabeledVector*): LinearRegressionModel = {
    val xDim = labeledPoints(0).vector.dimension
    val yDist:Gaussian = Gaussian()
    // Compute the average Vector
    val mean: Vector = {
      val svs = new StreamingVectorStats(xDim)
      labeledPoints.foreach(lv => {
        yDist(lv.y)
        svs(lv.vector)
      })
      svs.average()
    }

    val vX: MatrixValues = new MatrixValues(labeledPoints.size)
    val vY: MatrixValues = new MatrixValues(labeledPoints.size)
    var i = 0
    for (lv <- labeledPoints) {
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

    LinearRegressionModel(mean, beta, 1.0 - (errors*errors / yDist.variance))
  }

  def evaluate(lrt: LinearRegressionTest): LinearRegressionTestScore = {
    val model: LinearRegressionModel = train(lrt.data:_*)
    lrt.evaluate(model)
  }
}