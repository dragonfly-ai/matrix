package ai.dragonfly.math.matrix

import Jama.{Matrix, SingularValueDecomposition}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.*
import ai.dragonfly.math.vector.*

import scala.language.implicitConversions

object LinearRegressionSVD extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {

    val svd:SingularValueDecomposition = X.svd()

    // Construct Spsi, the Pseudo Inverse of S
    val singularValues: VectorValues = svd.getSingularValues()
    val Spsi:Matrix = Matrix.identity(singularValues.length, singularValues.length)
    for (i <- 0 until singularValues.length) Spsi.set(i, i, 1.0 / singularValues(i))

    // Compute Apsi, the Pseudo Inverse of A
    val Apsi: Matrix = svd.getU().times(Spsi).times(svd.getV().transpose()).transpose()

    // estimated Beta
    Apsi.times(Y)
  }


}
