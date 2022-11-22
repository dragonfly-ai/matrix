package verification.decomposition

import Jama.LUDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object LU extends Verification {

  override val name:String = "LU Decomposition"

  override def run: Unit = {

    val jlu: LUDecomposition = new LUDecomposition(jm)
    val mlu: matrix.decomposition.LU = matrix.decomposition.LU(mm)

    println(s"\tComparing Determinants: ${jlu.det()} vs ${mlu.det()} error = ${Math.abs(jlu.det() - mlu.det())}")

    println(s"\tComparing L : ${Verification.arrayCompare2D(jlu.getL.getArray, mlu.getL().values)}")
    println(s"\tComparing U : ${Verification.arrayCompare2D(jlu.getU.getArray, mlu.getU().values)}")

  }
}
