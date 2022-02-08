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

    TestPCA.testDimensionalityReduction()
  }

}
