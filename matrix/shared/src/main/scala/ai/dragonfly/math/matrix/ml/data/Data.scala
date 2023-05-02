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

package ai.dragonfly.math.matrix.ml.data

import narr.*
import ai.dragonfly.math.*
import interval.*
import Interval.*
import vector.{Vec, *}
import stats.{LabeledVec, SimpleLabeledVector}
import stats.probability.distributions.{EstimatedGaussian, stream}
import stream.StreamingVectorStats
import matrix.*
import matrix.util.*

import scala.language.{existentials, implicitConversions}

trait Data[N <: Int] {
  def dimension:Int

  val sampleSize:Int
  def X:Matrix
  //def asVectors:Set[Vector]
  def example(i:Int):Vec[N]
  def sampleMean:Vec[N]
  def sampleVariance:Vec[N]
  def sampleStandardDeviation:Vec[N]
  def domainComponent(i:Int):Interval[Double]
  def domainBias:Vec[N] = sampleMean
}

trait UnsupervisedData[N <: Int] extends Data[N] {

}

object StaticUnsupervisedData {
  inline def apply[N <: Int](examples:NArray[Vec[N]])(using ValueOf[N]):StaticUnsupervisedData[N] = {
    new StaticUnsupervisedData[N](examples)
  }
}

class StaticUnsupervisedData[N <: Int](examples:NArray[Vec[N]])(using ValueOf[N]) extends UnsupervisedData[N] {

  override val dimension: Int = examples(0).dimension

  override val sampleSize: Int = examples.length

  private val Xar:NArray[NArray[Double]] = NArray.ofSize[NArray[Double]](sampleSize)

  // Compute sample point statistics and populate Xar and X
  val temp = {
    val sampleVectorStats: StreamingVectorStats[N] = new stream.StreamingVectorStats[N]

    var i:Int = 0; while (i < sampleSize) {
      sampleVectorStats(examples(i))
      i += 1
    }

    val sampleMean:Vec[N] = sampleVectorStats.average()

    i = 0; while (i < sampleSize) {
      Xar(i) = (examples(i) - sampleMean).asInstanceOf[NArray[Double]]
      i += 1
    }

    val intervals:NArray[Interval[Double]] = NArray.ofSize[Interval[Double]](dimension)
    i = 0; while (i < dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
      i += 1
    }
    (sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }
  //(sampleMean:Vector, sampleVariance:Vector, sampleStandardDeviation:Vector, intervals:NArray[Interval[Double]]): Vector
  override val sampleMean:Vec[N] = temp._1
  override val sampleVariance:Vec[N] = temp._2
  override val sampleStandardDeviation:Vec[N] = temp._3
  val intervals:NArray[Interval[Double]] = temp._4

  override def domainComponent(i: Int):Interval[Double] = intervals(i)

  override val X: Matrix = Matrix(Xar)

  override def example(i: Int): Vec[N] =  Vec[N]( Xar(i) ) + sampleMean

}

trait SupervisedData[N <: Int] extends Data[N] {
  def y:Vec[sampleSize.type]
  def Y: Matrix
  def labeledExample(i:Int):LabeledVec[N]
  def labelStats:EstimatedGaussian
  def rangeBias:Double = labelStats.sampleMean
}

class StaticSupervisedData[N <: Int](labeledExamples:NArray[LabeledVec[N]])(using ValueOf[N]) extends SupervisedData[N] {

  override val dimension:Int = valueOf[N]

  override val sampleSize: Int = labeledExamples.length

  private val Xar:NArray[NArray[Double]] = NArray.ofSize[NArray[Double]](sampleSize)
  private val Yar:NArray[Double] = NArray.ofSize[Double](sampleSize)

  // Compute the average Vector
  val temp = {
    val labelStatsEstimator = stream.Gaussian()
    val sampleVectorStats:stream.StreamingVectorStats[N] = new stream.StreamingVectorStats[N]

    var i:Int = 0; while (i < sampleSize) {
      sampleVectorStats.apply(labeledExamples(i).vector, 1.0)
      labelStatsEstimator.observe(labeledExamples(i).y)
      i += 1
    }

    val sampleMean:Vec[N] = sampleVectorStats.average()
    val labelStats:EstimatedGaussian = labelStatsEstimator.estimate

    i = 0; while (i < sampleSize) {
      Xar(i) = (labeledExamples(i).vector - sampleMean).asInstanceOf[NArray[Double]]
      Yar(i) = labeledExamples(i).y - labelStats.sampleMean
      i += 1
    }

    val intervals:NArray[Interval[Double]] = NArray.ofSize[Interval[Double]](dimension)
    i = 0; while (i < dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
      i += 1
    }
    (labelStats, sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }

  override val labelStats:EstimatedGaussian = temp._1
  override val sampleMean:Vec[N] = temp._2
  override val sampleVariance:Vec[N] = temp._3
  override val sampleStandardDeviation:Vec[N] = temp._4

  val intervals: NArray[Interval[Double]] = temp._5

  override val y: Vec[sampleSize.type] = Vec[sampleSize.type](Yar)

  override val X: Matrix = Matrix(Xar)
  override val Y: Matrix = y.asColumnMatrix

  def example(i: Int): Vec[N] = Vec[N]( Xar(i) ) + sampleMean

  def labeledExample(i: Int): LabeledVec[N] = SimpleLabeledVector(Yar(i) + labelStats.sampleMean, example(i))

  def domainComponent(i: Int): Interval[Double] = intervals(i)

}