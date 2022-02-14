package ai.dragonfly.math.matrix

import ai.dragonfly.math.matrix.test.{LinearRegressionTest, LinearRegressionTestScore}
import ai.dragonfly.math.stats.LabeledVector

trait LinearRegression {
  def train(trainingData:Array[LabeledVector]): LinearRegressionModel

  def evaluate(lrt: LinearRegressionTest): LinearRegressionTestScore = {
    val model: LinearRegressionModel = train(lrt.trainingData)
    lrt.evaluate(model)
  }
}
