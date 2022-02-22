package ai.dragonfly.math.matrix.test

import Jama._
import ai.dragonfly.math.*
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
    println("\nSynthetic Tests: ")
    val slrt: SyntheticLinearRegressionTest = new SyntheticLinearRegressionTest(7, 100, 1000.0)
    println("\n\nTest LinearRegressionQR:")

    val syntProbLR: LinearRegressionProblem = LinearRegressionProblem(slrt.trainingData)

    val slrQR = LinearRegressionQR.train(syntProbLR)
    println(s"\tLinearRegressionQR.train(syntProbLR) => $slrQR")
    println(s"\tslrt.evaluate(slrQR) => ${slrt.evaluate(slrQR)}")

    println("\n\nTest LinearRegressionSVD:")
    val slrSVD = LinearRegressionSVD.train(syntProbLR)
    println(s"\tLinearRegressionSVD.train(syntProbLR) => $slrSVD")
    println(s"\tslrt.evaluate(slrSVD) => ${slrt.evaluate(slrSVD)}")

    println("\nEmperical Tests: ")
    val elrt: EmpiricalRegressionTest = new EmpiricalRegressionTest(EmpericalData.trainingData_01, EmpericalData.testData_01)
    println("\nTest LinearRegressionQR:")

    val emProbLR:LinearRegressionProblem = LinearRegressionProblem(EmpericalData.trainingData_01)

    val elrQR = LinearRegressionQR.train(emProbLR)
    println(s"\tLinearRegressionQR.train(emProbLR) => $elrQR")
    println(s"\tslrt.evaluate(elrQR) => ${elrt.evaluate(elrQR)}")

    println("\n\nTest LinearRegressionSVD:")
    val elrSVD = LinearRegressionSVD.train(emProbLR)
    println(s"\tLinearRegressionSVD.train(emProbLR) => $elrSVD")
    println(s"\tslrt.evaluate(elrSVD) => ${elrt.evaluate(elrSVD)}")

  }

}
