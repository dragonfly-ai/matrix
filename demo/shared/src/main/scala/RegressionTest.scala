import ai.dragonfly.math.*
import ai.dragonfly.math.Random.*
import ai.dragonfly.math.matrix.ml.data.*
import ai.dragonfly.math.matrix.ml.supervized.regression.*
import ai.dragonfly.math.stats.*
import ai.dragonfly.math.vector.*
import narr.*

trait LinearRegressionTest[V <: Vector] {
  def trainingData:SupervisedData[V]
  def testData:SupervisedData[V]
  def evaluate(model: LinearRegressionModel[V]):LinearRegressionTestScore
}

case class SyntheticLinearRegressionTest[V <: Vector](trueCoefficients: Vector, bias: Double, sampleSize:Int, noise:Double = 1.0) extends LinearRegressionTest[V] {
  val maxNorm:Double = trueCoefficients.dimension * trueCoefficients.magnitude //Math.min(2.0 * dimension, sampleSize)

  var syntheticError: Double = 0.0
  override val trainingData:SupervisedData[V] = {
    val td: NArray[LabeledVector[V]] = new NArray[LabeledVector[V]](sampleSize)

    var i:Int = 0; while (i < td.length) {
      val xi: V = defaultRandom.nextVector(trueCoefficients.dimension, maxNorm).asInstanceOf[V]
      val yi: Double = f(xi)

      val yi_noisy = yi + (noise * (defaultRandom.between(-0.5, 0.5)))

      td(i) = SimpleLabeledVector[V](yi_noisy, xi)
      val err = yi_noisy - yi

      syntheticError = syntheticError + err * err
      i += 1
    }
    new StaticSupervisedData(td)
  }

  syntheticError = Math.sqrt(syntheticError / trainingData.size)

  private def f(xi:Vector):Double = (xi dot trueCoefficients) + bias

  override def evaluate(model: LinearRegressionModel[V]):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    var i:Int = 0; while (i < testData.size) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - f(lv.x)
//      println(s"\ty = ${f(lv.x)} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
      i += 1
    }
    observedError = Math.sqrt( observedError / testData.size )
    LinearRegressionTestScore(model.standardError, observedError)
  }

  override def testData: SupervisedData[V] = trainingData
}


case class EmpiricalRegressionTest[V <: Vector](override val trainingData:SupervisedData[V], override val testData:SupervisedData[V]) extends LinearRegressionTest[V] {
  override def evaluate(model: LinearRegressionModel[V]):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    var i:Int = 0; while (i < testData.size) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - lv.y
//      println(s"\ty = ${lv.y} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
      i += 1
    }
    observedError = Math.sqrt(observedError / testData.size)
    LinearRegressionTestScore(model.standardError, observedError)
  }
}

case class LinearRegressionTestScore(standardError:Double, testError:Double) {}
