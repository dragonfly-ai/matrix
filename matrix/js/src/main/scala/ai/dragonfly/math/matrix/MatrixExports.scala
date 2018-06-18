package ai.dragonfly.math.matrix

import Jama._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

private object MatrixExports {

  @JSExportTopLevel("Jama.Matrix") val Matrix = js.constructorOf[Matrix]

  @JSExportTopLevel("Jama.SingularValueDecomposition") val SingularValueDecomposition = js.constructorOf[SingularValueDecomposition]

  @JSExportTopLevel("Jama.CholeskyDecomposition") val CholeskyDecomposition = js.constructorOf[CholeskyDecomposition]

  @JSExportTopLevel("Jama.EigenvalueDecomposition") val EigenvalueDecomposition = js.constructorOf[EigenvalueDecomposition]

  @JSExportTopLevel("Jama.LUDecomposition") val LUDecomposition = js.constructorOf[LUDecomposition]

  @JSExportTopLevel("Jama.QRDecomposition") val QRDecomposition = js.constructorOf[QRDecomposition]

}
