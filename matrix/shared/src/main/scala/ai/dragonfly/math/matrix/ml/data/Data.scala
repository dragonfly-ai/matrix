package ai.dragonfly.math.matrix.ml.data

import narr.*
import ai.dragonfly.math.*
import interval.*
import Interval.*
import vector.*
import stats.{LabeledVector, SimpleLabeledVector}
import stats.probability.distributions.{EstimatedGaussian, stream}
import stream.StreamingVectorStats
import matrix.*
import matrix.util.*

import scala.language.implicitConversions

trait Data[V <: Vector] {
  def dimension:Int
  def size:Int
  def X:Matrix
  //def asVectors:Set[Vector]
  def example(i:Int):V
  def sampleMean:V
  def sampleVariance:V
  def sampleStandardDeviation:V
  def domainComponent(i:Int):Interval[Double]
  def domainBias:V = sampleMean
}

trait UnsupervisedData[V <: Vector] extends Data[V] {

}

class StaticUnsupervisedData[V <: ai.dragonfly.math.vector.Vector](examples:NArray[V]) extends UnsupervisedData[V] {

  override val dimension: Int = examples(0).dimension
  override def size: Int = examples.length

  private val Xar:NArray[NArray[Double]] = NArray.ofSize[NArray[Double]](size)

  // Compute sample point statistics and populate Xar and X
  val temp = {
    val sampleVectorStats: StreamingVectorStats[V] = new stream.StreamingVectorStats[V](dimension)

    var i:Int = 0; while (i < size) {
      sampleVectorStats(examples(i))
      i += 1
    }

    val sampleMean:V = sampleVectorStats.average()

    i = 0; while (i < size) {
      Xar(i) = (examples(i) - sampleMean).values
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
  override val sampleMean:V = temp._1
  override val sampleVariance:V = temp._2
  override val sampleStandardDeviation:V = temp._3
  val intervals:NArray[Interval[Double]] = temp._4

  override def domainComponent(i: Int):Interval[Double] = intervals(i)

  override val X: Matrix = Matrix(Xar)

  override def example(i: Int): V =  (Vector( Xar(i) ) + sampleMean).asInstanceOf[V]

}

trait SupervisedData[V <: Vector] extends Data[V] {
  def y:V
  def Y: Matrix
  def labeledExample(i:Int):LabeledVector[V]
  def labelStats:EstimatedGaussian
  def rangeBias:Double = labelStats.sampleMean
}

class StaticSupervisedData[V <: Vector](labeledExamples:NArray[LabeledVector[V]]) extends SupervisedData[V] {

  override val dimension: Int = labeledExamples(0).vector.dimension
  override def size: Int = labeledExamples.length

  private val Xar:NArray[NArray[Double]] = NArray.ofSize[NArray[Double]](size)
  private val Yar:NArray[Double] = NArray.ofSize[Double](size)

  // Compute the average Vector
  val temp = {
    val labelStatsEstimator = stream.Gaussian()
    val sampleVectorStats:stream.StreamingVectorStats[V] = new stream.StreamingVectorStats[V](dimension)

    var i:Int = 0; while (i < size) {
      sampleVectorStats.apply(labeledExamples(i).vector, 1.0)
      labelStatsEstimator.observe(labeledExamples(i).y)
      i += 1
    }

    val sampleMean:V = sampleVectorStats.average()
    val labelStats:EstimatedGaussian = labelStatsEstimator.estimate

    i = 0; while (i < size) {
      Xar(i) = (labeledExamples(i).vector - sampleMean).values
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
  override val sampleMean:V = temp._2
  override val sampleVariance:V = temp._3
  override val sampleStandardDeviation:V = temp._4

  val intervals: NArray[Interval[Double]] = temp._5

  override val y: V = Vector(Yar).asInstanceOf[V]

  override val X: Matrix = Matrix(Xar)
  override val Y: Matrix = y.asColumnMatrix

  def example(i: Int): V = (Vector( Xar(i) ).asInstanceOf[V] + sampleMean).asInstanceOf[V]

  def labeledExample(i: Int): LabeledVector[V] = SimpleLabeledVector(Yar(i) + labelStats.sampleMean, example(i))

  def domainComponent(i: Int): Interval[Double] = intervals(i)

}