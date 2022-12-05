import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.ml.data.StaticSupervisedData
import ai.dragonfly.math.matrix.ml.supervized.regression.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.viz.cli.*

object DemoLinearRegression extends Demonstration {

  import ai.dragonfly.math.interval.*

  override def demo(): Unit = {

    println("\n\nLinear Regression Tests: ")
    println("\nSynthetic Tests: ")
    val slrt: SyntheticLinearRegressionTest[Vector2] = SyntheticLinearRegressionTest[Vector2](Vector2(2.0, 1.0), 2.0, 100, 1.1)
    println(s"Generated Synthetic Test Data: $slrt")

    val interval: Interval[Double] = `[]`[Double](-1.0, 15.0)

    val xPlot: Chart = Chart("X Component", "p.x", "f(p)", slrt.trainingData.domainComponent(0), interval, 100, 40)
    val yPlot: Chart = Chart("Y Component", "p.y", "f(p)", slrt.trainingData.domainComponent(1), interval, 100, 40)

    val xY = (0 until slrt.trainingData.size).map((i: Int) => {
      val lv = slrt.trainingData.labeledExample(i); Vector2(lv.vector.component(0), lv.y)
    })
    val yY = (0 until slrt.trainingData.size).map((i: Int) => {
      val lv = slrt.trainingData.labeledExample(i); Vector2(lv.vector.component(1), lv.y)
    })

    xPlot.scatter(" (p.x, f(p))", xY: _*)
    yPlot.scatter(" (p.y, f(p))", yY: _*)

    println("\nTest LinearRegressionQR:\n")

    val syntProbLR: LinearRegressionProblem[Vector2] = LinearRegressionProblem[Vector2](slrt.trainingData)

    val slrQR = LinearRegressionQR.train(syntProbLR)
    println(s"\tLinearRegressionQR.train(syntProbLR) => $slrQR\n")
    println(s"\tslrt.evaluate(slrQR) => ${slrt.evaluate(slrQR)}\n")

    val p = slrt.trainingData.sampleMean
    var yMean: Double = slrQR(p)

    val xSlopeQR = slrQR(p + Vector2(1.0, 0.0)) - yMean
    val ySlopeQR = slrQR(p + Vector2(0.0, 1.0)) - yMean

    xPlot.line(Vector2(p.component(0), yMean), xSlopeQR, "QR (p.x, f'(p))")
    yPlot.line(Vector2(p.component(1), yMean), ySlopeQR, "QR (p.y, f'(p))")

    println("\n\nTest LinearRegressionSVD:\n")
    val slrSVD = LinearRegressionSVD.train(syntProbLR)
    println(s"\tLinearRegressionSVD.train(syntProbLR) => $slrSVD\n")
    println(s"\tslrt.evaluate(slrSVD) => ${slrt.evaluate(slrSVD)}\n")

    yMean = slrSVD(p)
    val xSlopeSVD: Double = slrSVD(p + Vector2(1.0, 0.0)) - yMean
    val ySlopeSVD: Double = slrSVD(p + Vector2(0.0, 1.0)) - yMean

    val c = yMean / slrSVD.a.magnitude
    xPlot.line(Vector2(p.component(0), yMean), xSlopeSVD, "SVD (p.x, f'(p))")
    yPlot.line(Vector2(p.component(1), yMean), ySlopeSVD, "SVD (p.y, f'(p))")

    println(xPlot)
    println(yPlot)

    println("\nEmperical Tests:\n")
    val empericalTrainingData: StaticSupervisedData[VectorN] = new StaticSupervisedData[VectorN](EmpericalData.trainingData_01)
    val empericalTestData: StaticSupervisedData[VectorN] = new StaticSupervisedData[VectorN](EmpericalData.testData_01)
    val elrt: EmpiricalRegressionTest[VectorN] = EmpiricalRegressionTest[VectorN](empericalTrainingData, empericalTestData)
    val emProbLR: LinearRegressionProblem[VectorN] = LinearRegressionProblem[VectorN](empericalTrainingData)

    val size: Double = empericalTrainingData.size

    println("\nTest LinearRegressionQR:\n")
    val elrQR = LinearRegressionQR.train(emProbLR)
    println(s"\tLinearRegressionQR.train(emProbLR) => $elrQR\n")
    println(s"\tslrt.evaluate(elrQR) => ${elrt.evaluate(elrQR)}\n")

    println("\n\nTest LinearRegressionSVD:\n")
    val elrSVD = LinearRegressionSVD.train(emProbLR)
    println(s"\tLinearRegressionSVD.train(emProbLR) => $elrSVD\n")
    println(s"\tslrt.evaluate(elrSVD) => ${elrt.evaluate(elrSVD)}\n")

  }

  override def name: String = "QR and SVD Linear Regression: "

}
