import ai.dragonfly.math.example.Demonstrable
import ai.dragonfly.math.matrix.Matrix
import ai.dragonfly.math.vector.Vector4
import bridge.array.ARRAY
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

object DemoEigenValueDecomposition extends Demonstrable {
  def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {
    val M0: Matrix = Matrix(
      ARRAY[ARRAY[Double]](
        ARRAY[Double](1.0, 2.0, 3.0, 4.0),
        ARRAY[Double](0.0, -1.0, 0.0, -3.0),
        ARRAY[Double](4.0, 0.0, -7.0, 0.5),
        ARRAY[Double](0.27, ai.dragonfly.math.Constant.π, 1.1, 0.5),
      )
    )
    sb.append(M0.asString)
    sb.append("\n\n")
    sb.append(Vector4(M0.eig().getRealEigenvalues()))
    sb.append("\n\n")

  }

  def name: String = "DemoEigenValueDecomposition"
}
