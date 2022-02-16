package ai.dragonfly.math.matrix

import Jama.{Matrix, SingularValueDecomposition}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.stream.*
import ai.dragonfly.math.vector.*

import scala.language.implicitConversions

/*
 * Ported from: https://introcs.cs.princeton.edu/java/97data/MultipleLinearRegression.java.html
 * by Robert Sedgewick and Kevin Wayne.
*/

object LinearRegressionSVD extends LinearRegression {

  override def train(trainingData:Array[LabeledVector]): LinearRegressionModel = {
    val xDim = trainingData(0).vector.dimension
    val yDist:Gaussian = Gaussian()

    trainingData.foreach(lv => {
      yDist(lv.y)

    })

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

//    val svd:SingularValueDecomposition = X.transpose().times(X).times(1.0 / xDim).svd()
    val svd:SingularValueDecomposition = X.svd()

    // Construct Spsi, the Pseudo Inverse of S
    val singularValues: VectorValues = svd.getSingularValues()
    val Spsi:Matrix = Matrix.identity(singularValues.length, singularValues.length)
    for (i <- 0 until singularValues.length) Spsi.set(i, i, 1.0 / singularValues(i))

    // Compute Apsi, the Pseudo Inverse of A
    val Apsi: Matrix = svd.getU().times(Spsi).times(svd.getV().transpose()).transpose()
//    println(s"dim(Apsi) = ${Apsi.getRowDimension()} x ${Apsi.getColumnDimension()}")
//    println(s"dim(Y) = ${Y.getRowDimension()} x ${Y.getColumnDimension()}")
    val beta: Matrix = Apsi.times(Y)
//    println(s"dim(beta) = ${beta.getRowDimension()} x ${beta.getColumnDimension()}")
    val errors:Double = (X.times(beta).minus(Y)).norm2()

    LinearRegressionModel(beta, Math.sqrt(errors/trainingData.length), 1.0 - (errors*errors / (yDist.variance * yDist.sampleSize)))
  }

}
