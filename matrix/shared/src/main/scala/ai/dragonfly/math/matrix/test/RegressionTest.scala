package ai.dragonfly.math.matrix.test

import ai.dragonfly.math
import ai.dragonfly.math.matrix.LinearRegressionModel
import math.vector.Vector
import math.stats.LabeledVector

trait LinearRegressionTest {
  def trainingData:Array[LabeledVector]
  def testData:Array[LabeledVector]
  def evaluate(model: LinearRegressionModel):LinearRegressionTestScore
}

class SyntheticLinearRegressionTest(dimension:Int, sampleSize:Int, noise:Double = 1.0) extends LinearRegressionTest {
  val maxNorm = dimension * sampleSize

  val trueCoefficients: Vector = Vector.fill(dimension){ _ =>  Math.random() * maxNorm }
  val constant:Double = (Math.random() * maxNorm) / dimension

  override val trainingData: Array[LabeledVector] = new Array[LabeledVector](sampleSize)
  override def testData: Array[LabeledVector] = trainingData

  var syntheticError: Double = 0.0

  for (i <- 0 until trainingData.length) {
    val xi:Vector = Vector.random(dimension, maxNorm)

    val yi: Double = f(xi)
    val yi_noisy = yi + (noise * (Math.random() - 0.5))

    trainingData(i) = LabeledVector(yi_noisy, xi)

    val err = yi_noisy - yi
    syntheticError = syntheticError + err * err
  }

  syntheticError = Math.sqrt(syntheticError / trainingData.length)

  private def f(xi:Vector):Double = xi.dot(trueCoefficients) + constant

  override def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (i <- 0 until testData.length) {
      val lv = testData(i)
      val err = model(lv.x) - f(lv.x)
//      println(s"\ty = ${f(lv.x)} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    observedError = Math.sqrt( observedError / testData.length )
    LinearRegressionTestScore(model.standardError, observedError)
  }
}


case class EmpiricalRegressionTest(override val trainingData:Array[LabeledVector], override val testData:Array[LabeledVector]) extends LinearRegressionTest {
  override def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (lv <- testData) {
      val err = model(lv.x) - lv.y
//      println(s"\ty = ${lv.y} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    observedError = Math.sqrt(observedError / testData.length)
    LinearRegressionTestScore(model.standardError, observedError)
  }
}

case class LinearRegressionTestScore(standardError:Double, testError:Double) {}
