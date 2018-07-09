package ai.dragonfly.math.MatrixUtils.test

import Jama._
import ai.dragonfly.math.matrix.TestPCA

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("ai.dragonfly.matrix.tests.MatrixTest") @JSExportAll
object MatrixTest extends App {

  override def main(args: Array[String]): Unit = {
    test()
  }

  def test(): Unit = {
    println("hi?")
    val a = Array[Array[Double]](
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)
    )
    println(a)
    val m = new Matrix(a)
    println(m)
    val svd = new SingularValueDecomposition(m)
    println(svd)

    TestPCA.testDimensionalityReduction()
  }

}
