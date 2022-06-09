package ai.dragonfly.math.matrix.demo

import bridge.array.*
import ai.dragonfly.math.*
import vector.*
import stats.*
import Random.*
import matrix.LinearRegressionModel
import matrix.data.*

trait LinearRegressionTest {
  def trainingData:SupervisedData
  def testData:SupervisedData
  def evaluate(model: LinearRegressionModel):LinearRegressionTestScore
}

case class SyntheticLinearRegressionTest(trueCoefficients: Vector, bias: Double, sampleSize:Int, noise:Double = 1.0) extends LinearRegressionTest {
  val maxNorm:Double = trueCoefficients.dimension * trueCoefficients.magnitude //Math.min(2.0 * dimension, sampleSize)

  var syntheticError: Double = 0.0
  override val trainingData:SupervisedData = {
    val td: ARRAY[LabeledVector] = new ARRAY[LabeledVector](sampleSize)

    for (i <- td.indices) {
      val xi: Vector = defaultRandom.nextVector(trueCoefficients.dimension, maxNorm)
      val yi: Double = f(xi)

      val yi_noisy = yi + (noise * (defaultRandom.between(-0.5, 0.5)))

      td(i) = SimpleLabeledVector(yi_noisy, xi)
      val err = yi_noisy - yi

      syntheticError = syntheticError + err * err
    }
    new StaticSupervisedData(td)
  }

  syntheticError = Math.sqrt(syntheticError / trainingData.size)

  private def f(xi:Vector):Double = (xi dot trueCoefficients) + bias

  override def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (i <- 0 until testData.size) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - f(lv.x)
//      println(s"\ty = ${f(lv.x)} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    observedError = Math.sqrt( observedError / testData.size )
    LinearRegressionTestScore(model.standardError, observedError)
  }

  override def testData: SupervisedData = trainingData
}


case class EmpiricalRegressionTest(override val trainingData:SupervisedData, override val testData:SupervisedData) extends LinearRegressionTest {
  override def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (i <- 0 until testData.size) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - lv.y
//      println(s"\ty = ${lv.y} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    observedError = Math.sqrt(observedError / testData.size)
    LinearRegressionTestScore(model.standardError, observedError)
  }
}

case class LinearRegressionTestScore(standardError:Double, testError:Double) {}
