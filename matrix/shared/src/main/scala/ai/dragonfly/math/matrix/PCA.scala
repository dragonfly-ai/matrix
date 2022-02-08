package ai.dragonfly.math.matrix

import scala.language.implicitConversions

import Jama.{Matrix, SingularValueDecomposition}
import ai.dragonfly.math
import math.vector._
import math.stats.stream.StreamingVectorStats

import ai.dragonfly.math.matrix.MatrixUtils.*
import scala.collection.mutable.ListBuffer

object PCA {

  // Create a PCA object from a set of data points.
  def apply(points: VECTORS): PCA = {
    val dim = points(0).dimension

    // Compute the average Vector

    val svs = new StreamingVectorStats(dim)

    for (p <- points) svs(p)

    val mean: Vector = svs.average()

    // arrange the matrix of centered points.
    val mArr = new MatrixValues(points.length)

    for (i <- points.indices) {
      mArr(i) = points(i).copy().subtract(mean).values
    }

    val X = new Matrix(mArr)

    // Computer Singular Value Decomposition

    new PCA(
      X.transpose().times(X).times(1.0 / points.length).svd(),
      mean,
      dim
    )
  }

}

case class PCA (svd: SingularValueDecomposition, mean: Vector, dimension: Double) {

  val U = svd.getU()

  def getReducer(k: Int): DimensionalityReducerPCA = {

    DimensionalityReducerPCA(
      U.getMatrix(0, U.getRowDimension() - 1, 0, k-1),
      mean,
      k
    )
  }

  def getRankedBasisPairs: Seq[BasisPair] = {
    val size = svd.getU().getRowDimension()
    val dim = svd.getU().getColumnDimension()
    val singularValues = svd.getSingularValues()
    var pairs: ListBuffer[BasisPair] = new ListBuffer[BasisPair]()
    val m: Matrix = svd.getU()

    for (i <- 0 until size) {
      val vectorValues = new Array[Double](dim)

      for (j <- 0 until dim) vectorValues(j) = m.get(i, j)

      pairs = pairs += BasisPair(
        singularValues(i),
        Vector(vectorValues:_*)
      )
    }
    pairs.toList
  }

}

case class BasisPair (variance: Double, basisVector: Vector)

import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Vector_Matrix
import ai.dragonfly.math.matrix.MatrixUtils.given_Conversion_Matrix_Vector
case class DimensionalityReducerPCA(U: Matrix, mean: Vector, k: Int) {
  //println(U.getRowDimension + " " + U.getColumnDimension)
  def project(v: Vector): Vector = {
    val vM: Matrix = v.copy().subtract(mean)
    //println(vM.getRowDimension + " " + vM.getColumnDimension)
    U.transpose().times(vM)
  }

  //  Recover matrix from projection onto reduced principle components
//  def recover(Z, Ureduce) {
//    return numeric.dot(Z, numeric.transpose(Ureduce))
//  }
}

object TestPCA {

  def testDimensionalityReduction(): Unit = {
    val vArr = new VECTORS(100); for (i <- vArr.indices) vArr(i) = Vector.random(3)
    val pca = PCA (vArr)
    val reducer = pca.getReducer(2)

    val basisPairs = pca.getRankedBasisPairs
    println(s"basisPairs: $basisPairs")

    for (v <- vArr) println(s"$v -> ${reducer.project (v)}")

  }

}