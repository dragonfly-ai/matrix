package verification.decomposition

import Jama.QRDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object QR extends Verification {

  override val name:String = "QR Decomposition"

  override def run: Unit = {

    val jqr:QRDecomposition = new QRDecomposition(jm)
    val mqr:matrix.decomposition.QR[11, 11] = matrix.decomposition.QR[11, 11](mm)

    println(s"\tComparing isFullRank: ${jqr.isFullRank} vs ${mqr.isFullRank()}")

    println(s"\tComparing Q : ${Verification.arrayCompare2D(jqr.getQ.getArray, mqr.getQ().getArray())}")
    println(s"\tComparing H : ${Verification.arrayCompare2D(jqr.getH.getArray, mqr.getH().getArray())}")
    println(s"\tComparing R : ${Verification.arrayCompare2D(jqr.getR.getArray, mqr.getR().getArray())}")

  }
}
