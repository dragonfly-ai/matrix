/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.ml.data.{StaticSupervisedData, SupervisedData}
import ai.dragonfly.math.matrix.ml.supervized
import ai.dragonfly.math.matrix.ml.supervized.regression
import ai.dragonfly.math.stats.LabeledVec
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.vector.*

trait LinearRegression {

  def estimateBeta(X:Matrix, Y:Matrix): Matrix

  def train[N <: Int](lrp:LinearRegressionProblem[N]): LinearRegressionModel[N] = {
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

    regression.LinearRegressionModel[N](
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ))
    )
  }
}

trait LinearRegressionProblem[N <: Int] {
  val dimension:Int
  val X: Matrix
  val Y: Matrix
  val bias:Double
  val mean:Vec[N]
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ
}

object LinearRegressionProblem {

  def apply[N <: Int](trainingData:SupervisedData[N])(using ValueOf[N]):LinearRegressionProblem[N] = {
    new LinearRegressionProblem[N] {
      override val dimension: Int = valueOf[N]
      override val X: Matrix = trainingData.X
      override val Y: Matrix = trainingData.Y
      override val mean:Vec[N] = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


