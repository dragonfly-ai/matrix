package ai.dragonfly.math.matrix.ml.data

import bridge.array.*
import ai.dragonfly.math.*
import interval.*
import Interval.*
import vector.*
import stats.{LabeledVector, SimpleLabeledVector}
import stats.probability.distributions.{EstimatedGaussian, stream}
import matrix.*
import matrix.util.*

import scala.language.implicitConversions

trait Data {
  def dimension:Int
  def size:Int
  def X:Matrix
  //def asVectors:Set[Vector]
  def example(i:Int):Vector
  def sampleMean:Vector
  def sampleVariance:Vector
  def sampleStandardDeviation:Vector
  def domainComponent(i:Int):Interval[Double]
  def domainBias:Vector = sampleMean
}

trait UnsupervisedData extends Data {

}

class StaticUnsupervisedData(examples:ARRAY[Vector]) extends UnsupervisedData {

  override val dimension: Int = examples(0).dimension
  override def size: Int = examples.length

  private val Xar:ARRAY[ARRAY[Double]] = new ARRAY[ARRAY[Double]](size)

  // Compute sample point statistics and populate Xar and X
  val temp = {
    val sampleVectorStats = new stream.StreamingVectorStats(dimension)

    for (i <- 0 until size) sampleVectorStats(examples(i))

    val sampleMean = sampleVectorStats.average()

    for (i <- 0 until size) Xar(i) = (examples(i) - sampleMean).values

    val intervals:ARRAY[Interval[Double]] = new ARRAY(dimension)
    for (i <- 0 until dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
    }
    (sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }
  //(sampleMean:Vector, sampleVariance:Vector, sampleStandardDeviation:Vector, intervals:ARRAY[Interval[Double]]): Vector
  override val sampleMean:Vector = temp._1
  override val sampleVariance:Vector = temp._2
  override val sampleStandardDeviation:Vector = temp._3
  val intervals:ARRAY[Interval[Double]] = temp._4

  override def domainComponent(i: Int):Interval[Double] = intervals(i)

  override val X: Matrix = Matrix(Xar)

  override def example(i: Int): Vector =  Vector( Xar(i) ) + sampleMean

}

trait SupervisedData extends Data {
  def y:Vector
  def Y: Matrix
  def labeledExample(i:Int):LabeledVector
  def labelStats:EstimatedGaussian
  def rangeBias:Double = labelStats.sampleMean
}

class StaticSupervisedData(labeledExamples:ARRAY[LabeledVector]) extends SupervisedData {

  override val dimension: Int = labeledExamples(0).vector.dimension
  override def size: Int = labeledExamples.length

  private val Xar:ARRAY[ARRAY[Double]] = new ARRAY[ARRAY[Double]](size)
  private val Yar:ARRAY[Double] = new ARRAY[Double](size)

  // Compute the average Vector
  val temp = {
    val labelStatsEstimator = stream.Gaussian()
    val sampleVectorStats = new stream.StreamingVectorStats(dimension)

    for (i <- 0 until size) {
      sampleVectorStats(labeledExamples(i).vector)
      labelStatsEstimator.observe(labeledExamples(i).y)
    }

    val sampleMean = sampleVectorStats.average()
    val labelStats:EstimatedGaussian = labelStatsEstimator.estimate

    for (i <- 0 until size) {
      Xar(i) = (labeledExamples(i).vector - sampleMean).values
      Yar(i) = labeledExamples(i).y - labelStats.sampleMean
    }

    val intervals:ARRAY[Interval[Double]] = new ARRAY(dimension)
    for (i <- 0 until dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
    }
    (labelStats, sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }

  override val labelStats:EstimatedGaussian = temp._1
  override val sampleMean:Vector = temp._2
  override val sampleVariance:Vector = temp._3
  override val sampleStandardDeviation:Vector = temp._4

  val intervals: ARRAY[Interval[Double]] = temp._5

  override val y: Vector = Vector(Yar)

  override val X: Matrix = Matrix(Xar)
  override val Y: Matrix = y.asColumnMatrix

  def example(i: Int): Vector = Vector( Xar(i) ) + sampleMean

  def labeledExample(i: Int): LabeledVector = SimpleLabeledVector(Yar(i) + labelStats.sampleMean, example(i))

  def domainComponent(i: Int): Interval[Double] = intervals(i)

}