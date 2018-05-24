package ai.dragonfly.math.matrix



import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("ai.dragonfly.math.matrix.PCA") @JSExportAll
object PCA {
/*
  // Return matrix of all principle components as column vectors
  def pca(pointMatrix: Matrix): Matrix = {
    val m = pointMatrix.getArray.length
    pointMatrix.transpose.times(pointMatrix).times(1.0 / m).svd.getU
  }
*/
  // Return matrix of k first principle components as column vectors
  // def pcaReduce(u:Matrix, k:Int): Matrix = new Matrix(u.getArray.slice(0, k))
/*
  // Project matrix X onto reduced principle components matrix
  def pcaProject(v: Matrix, Ureduce) {
    numeric.dot(X, Ureduce)
  }

  // Recover matrix from projection onto reduced principle components
  def pcaRecover(Z, Ureduce) {
    numeric.dot(Z, numeric.transpose(Ureduce))
  }
*/
}
