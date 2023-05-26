package verification.decomposition


import Jama.CholeskyDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object Cholesky extends Verification {

  override val name: String = "Cholesky Decomposition"

  override def run: Unit = {

    val jCh: CholeskyDecomposition = new CholeskyDecomposition(jm)
    val mCh: matrix.decomposition.Cholesky[11] = matrix.decomposition.Cholesky[11, 11](mm)

    println(s"\tComparing L : ${Verification.arrayCompare2D(jCh.getL.getArray, mCh.getL().getArray())}")

  }
}
