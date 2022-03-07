package ai.dragonfly.math.matrix

import Jama.{Matrix, QRDecomposition}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.stats.probability.distributions.stream.*
import ai.dragonfly.math.vector.*
import util.given_Dimensioned_Matrix

import scala.language.implicitConversions

/*
 * Ported from: https://introcs.cs.princeton.edu/java/97data/MultipleLinearRegression.java.html
 * by Robert Sedgewick and Kevin Wayne.
*/

object LinearRegressionQR extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {
    val QRD: QRDecomposition = new QRDecomposition(X)
//    println(s"${X.dim} ${Y.dim}")
    QRD.solve(Y)
  }

}