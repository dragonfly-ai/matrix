package ai.dragonfly.math.matrix.test

import ai.dragonfly.math
import ai.dragonfly.math.matrix.LinearRegressionModel
import math.vector.Vector
import math.stats.LabeledVector

trait LinearRegressionTest {
  def evaluate(model: LinearRegressionModel):LinearRegressionTestScore
}

class SyntheticLinearRegressionTest(dimension:Int, sampleSize:Int, noise:Double = 1.0) {
  val maxNorm = dimension * sampleSize

  val trueCoefficients: Vector = Vector.fill(dimension){ _ =>  Math.random() * maxNorm }
  val constant:Double = (Math.random() * maxNorm) / dimension

  val data: Array[LabeledVector] = new Array[LabeledVector](sampleSize)
  val error: Array[Double] = new Array[Double](sampleSize)

  var syntheticError: Double = 0.0

  for (i <- 0 until data.length) {
    val xi:Vector = Vector.random(dimension, maxNorm)

    val yi: Double = f(xi)
    val yi_noisy = yi + (noise * (Math.random() - 0.5))

    data(i) = LabeledVector(yi_noisy, xi)

    val err = yi_noisy - yi
    error(i) = err * err
    syntheticError = syntheticError + error(i)
  }

  private def f(xi:Vector):Double = xi.dot(trueCoefficients) + constant

  def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (i <- 0 until data.length) {
      val lv = data(i)
      val err = model(lv.x) - f(lv.x)
      println(s"\ty = ${f(lv.x)} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    LinearRegressionTestScore(syntheticError, observedError)
  }
}


case class EmpiricalRegressionTest(testData:Iterable[LabeledVector]) {
  def evaluate(model: LinearRegressionModel):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    for (lv <- testData) {
      val err = model(lv.x) - lv.y
      println(s"\ty = ${lv.y} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
    }
    LinearRegressionTestScore(model.error, observedError)
  }
}

case class LinearRegressionTestScore(trueError:Double, observedError:Double) {}
