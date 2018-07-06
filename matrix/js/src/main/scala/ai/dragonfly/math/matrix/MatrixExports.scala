package ai.dragonfly.math.MatrixUtils

import Jama.js._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

private object MatrixExports {

  @JSExportTopLevel("Jama.js.Matrix") val Matrix = js.constructorOf[Matrix]

  @JSExportTopLevel("Jama.js.SingularValueDecomposition") val SingularValueDecomposition = js.constructorOf[SingularValueDecomposition]

  @JSExportTopLevel("Jama.js.CholeskyDecomposition") val CholeskyDecomposition = js.constructorOf[CholeskyDecomposition]

  @JSExportTopLevel("Jama.js.EigenvalueDecomposition") val EigenvalueDecomposition = js.constructorOf[EigenvalueDecomposition]

  @JSExportTopLevel("Jama.js.LUDecomposition") val LUDecomposition = js.constructorOf[LUDecomposition]

  @JSExportTopLevel("Jama.js.QRDecomposition") val QRDecomposition = js.constructorOf[QRDecomposition]

}
