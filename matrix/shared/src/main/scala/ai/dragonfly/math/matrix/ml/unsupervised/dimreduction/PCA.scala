package ai.dragonfly.math.matrix.ml.unsupervised.dimreduction

import ai.dragonfly.math.*
import ai.dragonfly.math.geometry.Line
import ai.dragonfly.math.matrix.util.*
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.decomposition.SV
import ai.dragonfly.math.matrix.ml.data.*
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.*
import narr.*

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object PCA {

  // Create a PCA object from a set of data points
  def apply(data: UnsupervisedData): PCA = {

    // arrange the matrix of centered points
    val Xc = Matrix(
      NArray.tabulate[NArray[Double]](data.size)(
        (row:Int) => (data.example(row) - data.sampleMean).values
      )
    )

    new PCA(
      Xc.transpose().times(Xc).times(1.0 / data.size).svd(), // Compute Singular Value Decomposition
      data.sampleMean,
      data.dimension
    )
  }


}

case class PCA (svd: SV, mean: Vector, dimension: Double) {

  lazy val Uᵀ:Matrix = svd.getU().transpose()

  def getReducer(k: Int): DimensionalityReducerPCA = DimensionalityReducerPCA(Matrix(Uᵀ.getArray().take(k)), mean)

  lazy val basisPairs: Seq[BasisPair] = {
    val singularValues = svd.getSingularValues()
    val arr: NArray[NArray[Double]] = Uᵀ.getArray()
    var pairs:Seq[BasisPair] = Seq[BasisPair]()
    for (i <- arr.indices) {
      pairs = pairs :+ BasisPair(
        singularValues(i),
        Vector(arr(i))
      )
    }
    pairs
  }
}

case class BasisPair (variance: Double, basisVector: Vector)

case class DimensionalityReducerPCA(Ak:Matrix, mean: Vector) {

  /**
   * Reduce dimensionality of vector from domainDimension to rangeDimension
   * @param v domainDimensioned vector
   * @return rangeDimensioned vector
   */
  def apply(v:Vector):Vector = (Ak * (v - mean).asColumnMatrix).asVector

  def domainDimension:Int = mean.dimension

  def rangeDimension:Int = Ak.getRowDimension()

  /**
   * Approximate inverse of dimensionality reduction
   *
   * @param v rangeDimensioned vector
   * @return rangeDimensioned vector
   */
  def unapply(v:Vector):Vector = (v.asRowMatrix * Ak).asVector + mean

}
