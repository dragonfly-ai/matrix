package ai.dragonfly.jama.test

import Jama.Matrix

import ai.dragonfly.math.matrix._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("MatrixTest")
object MatrixTest extends App {

  override def main(args: Array[String]): Unit = {
    println("hi?")
    val a = Array[Array[Double]](
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
      Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)
    )
    println(a)
    println(new Matrix(a))
  }

  def testSVD(): Matrix = {
    val m = new Matrix(
      js.Array[js.Array[Double]](
        js.Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
        js.Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5),
        js.Array[Double](Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5)
      )
    )

    m.svd.getU
  }
}
