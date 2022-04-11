package ai.dragonfly.math.matrix

import Jama.Matrix
import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.data.{StaticSupervisedData, SupervisedData}
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.demo.{EmpericalData, EmpiricalRegressionTest, LinearRegressionTest, LinearRegressionTestScore, SyntheticLinearRegressionTest}
import ai.dragonfly.math.stats.LabeledVector
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.visualization

trait LinearRegression {

  def estimateBeta(X:Matrix, Y:Matrix): Matrix

  def train(lrp:LinearRegressionProblem): LinearRegressionModel = {
    import lrp.*

    val A:Matrix = estimateBeta(X, Y)

    val errors:Matrix = X.times(A).minus(Y)

    var err:Double = 0.0
    var `err²`: Double = 0.0
    for (e <- errors.getRowPackedCopy()) {
      val et:Double = e*e
      err = err + Math.sqrt(et)
      `err²` = `err²` + et
    }

    LinearRegressionModel(
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ))
    )
  }
}

trait LinearRegressionProblem {
  val X: Matrix
  val Y: Matrix
  val bias:Double
  val mean:Vector
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ
}

object LinearRegressionProblem {

  def apply(trainingData:SupervisedData):LinearRegressionProblem = {
    new LinearRegressionProblem {
      override val X: Matrix = trainingData.X
      override val Y: Matrix = trainingData.Y
      override val mean:Vector = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


import ai.dragonfly.math.example.Demonstrable

object DemoLinearRegression extends Demonstrable {

  import ai.dragonfly.math.interval.*

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {

    sb.append("\n\nLinear Regression Tests: ")
    sb.append("\nSynthetic Tests: ")
    val slrt: SyntheticLinearRegressionTest = SyntheticLinearRegressionTest(Vector2(2.0, 1.0), 2.0, 100, 1.1)
    sb.append(s"Generated Synthetic Test Data: $slrt")

    val interval:Interval[Double] = `[]`[Double](-1.0, 15.0)

    val xPlot:visualization.Chart = visualization.Chart( "X Component", "p.x", "f(p)", slrt.trainingData.domainComponent(0), interval, 100, 40 )
    val yPlot:visualization.Chart = visualization.Chart( "Y Component", "p.y", "f(p)", slrt.trainingData.domainComponent(1), interval, 100, 40 )

    val xY = (0 until slrt.trainingData.size).map((i:Int) => { val lv = slrt.trainingData.labeledExample(i); Vector2(lv.vector.component(0), lv.y) })
    val yY = (0 until slrt.trainingData.size).map((i:Int) => { val lv = slrt.trainingData.labeledExample(i); Vector2(lv.vector.component(1), lv.y) })

    xPlot.scatter(" (p.x, f(p))", xY:_*)
    yPlot.scatter(" (p.y, f(p))", yY:_*)

    sb.append("\nTest LinearRegressionQR:\n")

    val syntProbLR: LinearRegressionProblem = LinearRegressionProblem(slrt.trainingData)

    val slrQR = LinearRegressionQR.train(syntProbLR)
    sb.append(s"\tLinearRegressionQR.train(syntProbLR) => $slrQR\n")
    sb.append(s"\tslrt.evaluate(slrQR) => ${slrt.evaluate(slrQR)}\n")

    val p = slrt.trainingData.sampleMean
    var yMean:Double = slrQR(p)

    val xSlopeQR = slrQR(p + Vector2(1.0, 0.0)) - yMean
    val ySlopeQR = slrQR(p + Vector2(0.0, 1.0)) - yMean

    xPlot.line(Vector2(p.component(0), yMean), xSlopeQR, "QR (p.x, f'(p))")
    yPlot.line(Vector2(p.component(1), yMean), ySlopeQR, "QR (p.y, f'(p))")

    sb.append("\n\nTest LinearRegressionSVD:\n")
    val slrSVD = LinearRegressionSVD.train(syntProbLR)
    sb.append(s"\tLinearRegressionSVD.train(syntProbLR) => $slrSVD\n")
    sb.append(s"\tslrt.evaluate(slrSVD) => ${slrt.evaluate(slrSVD)}\n")

    yMean = slrSVD(p)
    val xSlopeSVD:Double = slrSVD(p + Vector2(1.0, 0.0)) - yMean
    val ySlopeSVD:Double = slrSVD(p + Vector2(0.0, 1.0)) - yMean

    val c = yMean / slrSVD.a.magnitude()
    xPlot.line(Vector2(p.component(0), yMean), xSlopeSVD, "SVD (p.x, f'(p))")
    yPlot.line(Vector2(p.component(1), yMean), ySlopeSVD, "SVD (p.y, f'(p))")

    sb.append(xPlot)
    sb.append(yPlot)

    sb.append("\nEmperical Tests:\n")
    val empericalTrainingData: StaticSupervisedData = new StaticSupervisedData(EmpericalData.trainingData_01)
    val empericalTestData: StaticSupervisedData = new StaticSupervisedData(EmpericalData.testData_01)
    val elrt: EmpiricalRegressionTest = EmpiricalRegressionTest(empericalTrainingData, empericalTestData)
    val emProbLR:LinearRegressionProblem = LinearRegressionProblem(empericalTrainingData)

    val size:Double = empericalTrainingData.size

    sb.append("\nTest LinearRegressionQR:\n")
    val elrQR = LinearRegressionQR.train(emProbLR)
    sb.append(s"\tLinearRegressionQR.train(emProbLR) => $elrQR\n")
    sb.append(s"\tslrt.evaluate(elrQR) => ${elrt.evaluate(elrQR)}\n")

    sb.append("\n\nTest LinearRegressionSVD:\n")
    val elrSVD = LinearRegressionSVD.train(emProbLR)
    sb.append(s"\tLinearRegressionSVD.train(emProbLR) => $elrSVD\n")
    sb.append(s"\tslrt.evaluate(elrSVD) => ${elrt.evaluate(elrSVD)}\n")

  }

  override def name:String = "QR and SVD Linear Regression: "

}