package ai.dragonfly.math.matrix.test

import Jama._
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*

object MatrixTest {

  def main(args: Array[String]): Unit = {
    test()
  }

  def test(): Unit = {
    val a = new MatrixValues(3)
    a(0) = VectorValues(Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)
    a(1) = VectorValues(Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)
    a(2) = VectorValues(Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)

    println(a)
    val m = new Matrix(a)
    println(m)
    val svd = new SingularValueDecomposition(m)
    println(svd)

    println("\n\nTest PCA:")
    TestPCA()

    println("\n\nLinear Regression Tests: ")
    val lrt: SyntheticLinearRegressionTest = new SyntheticLinearRegressionTest(7, 100, 1000.0)
    println("\n\nTest LinearRegressionQR:")
    println(s"\tLinearRegressionQR.evaluate(lrt) => ${LinearRegressionQR.evaluate(lrt)}")
    println("\n\nTest LinearRegressionSVD:")
    println(s"\tLinearRegressionSVD.evaluate(lrt) => ${LinearRegressionSVD.evaluate(lrt)}")
  }

}
