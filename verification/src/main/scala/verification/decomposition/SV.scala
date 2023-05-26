package verification.decomposition

import Jama.SingularValueDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object SV extends Verification {

  override val name:String = "Singular Value Decomposition"

  override def run: Unit = {

    val jsvd:SingularValueDecomposition = new SingularValueDecomposition(jm)
    val msvd:matrix.decomposition.SV[11, 11, 11] = matrix.decomposition.SV[11, 11, 11](mm)

    println(s"\tComparing Two norm condition number: ${jsvd.cond()} vs ${msvd.cond()} error = ${Math.abs(jsvd.cond() - msvd.cond())}")
    println(s"\tComparing Norm2: ${jsvd.norm2()} vs ${msvd.norm2()} error = ${Math.abs(jsvd.norm2() - msvd.norm2())}")
    println(s"\tComparing Rank: ${jsvd.rank()} vs ${msvd.rank()} error = ${Math.abs(jsvd.rank() - msvd.rank())}")


    println(s"\tComparing Singular Values: ${Verification.arrayCompare(jsvd.getSingularValues, msvd.getSingularValues().asInstanceOf[Array[Double]])}")

    println(s"\tComparing S : ${Verification.arrayCompare2D(jsvd.getS.getArray, msvd.getS().getArray())}")
    println(s"\tComparing U : ${Verification.arrayCompare2D(jsvd.getU.getArray, msvd.getU().getArray())}")
    println(s"\tComparing V : ${Verification.arrayCompare2D(jsvd.getV.getArray, msvd.getV().getArray())}")

  }

}
