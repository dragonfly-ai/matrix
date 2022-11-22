import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.matrix.Matrix
import ai.dragonfly.math.vector.Vector4
import narr.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

object DemoEigenDecomposition extends Demonstration {
  def demo(): Unit = {
    val M0: Matrix = Matrix(
      NArray[NArray[Double]](
        NArray[Double](1.0, 2.0, 3.0, 4.0),
        NArray[Double](0.0, -1.0, 0.0, -3.0),
        NArray[Double](4.0, 0.0, -7.0, 0.5),
        NArray[Double](0.27, ai.dragonfly.math.Constant.π, 1.1, 0.5),
      )
    )
    println(M0.asString)
    println("\n\n")
    println(Vector4(M0.eig().getRealEigenvalues()))
    println("\n\n")

  }

  def name: String = "DemoEigenValueDecomposition"
}
