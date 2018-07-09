package ai.dragonfly.math.matrix

import Jama.{Matrix, SingularValueDecomposition}
import ai.dragonfly.math.stats.StreamingVectorStats
import ai.dragonfly.math.vector._
import ai.dragonfly.math.matrix.MatrixUtils._

object PCA {

  // Create a PCA object from a set of data points.
  def apply(points: Array[Vector]): PCA = {
    val dim = points(0).copy().dimension

    // Compute the average Vector

    val svs = new StreamingVectorStats(dim)

    for (p <- points) svs(p)

    val mean: Vector = svs.average()

    // arrange the matrix of centered points.
    val mArr = new Array[Array[Double]](points.length)

    for (i <- points.indices) {
      mArr(i) = points(i).copy().subtract(mean).values
    }

    val X = new Matrix(mArr)

    // Computer Singular Value Decomposition

    new PCA(
      X.transpose().times(X).times(1.0 / points.length).svd,
      mean,
      dim
    )
  }

}

case class PCA (svd: SingularValueDecomposition, mean: Vector, dimension: Double) {

  def getReducer(k: Int): DimensionalityReducerPCA = {
    val U = svd.getU()
    DimensionalityReducerPCA(
      U.getMatrix(0, U.getRowDimension() - 1, 0, k-1),
      mean,
      k
    )
  }

}

case class DimensionalityReducerPCA(U: Matrix, mean: Vector, k: Int) {
  //println(U.getRowDimension + " " + U.getColumnDimension)
  def project(v: Vector): Vector = {

    val vM: Matrix = v.copy().subtract(mean)
    //println(vM.getRowDimension + " " + vM.getColumnDimension)
    U.transpose.times(vM)
  }

  //  Recover matrix from projection onto reduced principle components
//  def recover(Z, Ureduce) {
//    return numeric.dot(Z, numeric.transpose(Ureduce))
//  }
}

object TestPCA {

  def testDimensionalityReduction(): Unit = {
    val vArr = Array.fill[Vector] (100) (VectorN.random (3) )
    val reducer = PCA (vArr).getReducer(2)

    for (v <- vArr) {
      println(s"$v -> ${reducer.project (v)}")
    }
  }

}