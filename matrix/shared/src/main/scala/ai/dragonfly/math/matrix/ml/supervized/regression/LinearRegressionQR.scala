package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.matrix.decomposition.QR
import ai.dragonfly.math.matrix.*

object LinearRegressionQR extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {
    val QRD: QR = QR(X)
    //    println(s"${X.dim} ${Y.dim}")
    QRD.solve(Y)
  }

}
