package ai.dragonfly.math.matrix

import Jama.Matrix
import scala.scalajs.js
import ai.dragonfly.math.vector._

object MatrixUtils {

  given Conversion[Vector, Matrix] with
    def apply(v: Vector): Matrix = {
      val marr = new MatrixValues(v.dimension)
      for (i <- v.values.indices) {
        marr(i) = VectorValues.fill(1)(_ => v.values(i))
      }
      new Matrix(marr)
    }

  given Conversion[Matrix, Vector] with
    def apply(m: Matrix): Vector = {
      if (m.getColumnDimension() == 1) {
        Vector(m.getRowPackedCopy())
      } else throw ai.dragonfly.math.vector.MismatchedVectorDimensionsException(Vector.random(m.getRowDimension()), Vector.random(m.getRowDimension() * m.getColumnDimension()))
    }

}
