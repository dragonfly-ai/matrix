package ai.dragonfly.math.matrix

import Jama.Matrix
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.vector.*

package object util {



  trait Dimensioned[M] {
    extension (m: M) def dim: String
    extension (m: M) def *(m1:Matrix):Matrix
  }

  given Dimensioned[Matrix] with
    extension (m: Matrix) def dim: String = s"dim(${m.getRowDimension()}x${m.getColumnDimension()})"
    extension (m: Matrix) def *(m1:Matrix):Matrix = m.times(m1)


//  extension (a: Matrix) def *(b: Matrix): Matrix = a.times(b)

  extension (a: Matrix) def asVector: Vector = {
      if (a.getColumnDimension() == 1 || a.getRowDimension() == 1) {
        Vector(a.getRowPackedCopy())
      } else throw CannotExpressMatrixAsVector(a)
    }

  extension (a: Vector) def asRowMatrix: Matrix = new Matrix(a.values, 1)

  extension (a: Vector) def asColumnMatrix: Matrix = new Matrix(a.values, a.dimension)


  //  given Multiplies[Double, Vector, Vector] with
//    extension (a: Double) def *(b: Vector): Vector = {
//      import ai.dragonfly.math.vector.given_VectorOps_Vector
//      b.scale(a)
//    }
/*

  given Multiplies[Matrix, Vector, Vector] with
    extension (a: Matrix) def *(b: Vector): Vector = ???
*/

//  given Multiplies[Vector, Matrix, Vector] with
//    extension (a: Vector) def *(b: Matrix): Vector = {
//      Vector((new Matrix(a.values, a.dimension)).times(b).getColumnPackedCopy())
//    }
//
//  given ForceMultiplier[Matrix, Vector] with
//    extension (m: Matrix) def *(v: Vector): Vector = {
//      val marr: MatrixValues = m.getArray()
//      val values: VectorValues = new VectorValues(marr.length)
//      for (i <- marr.indices) {
//        values(i) = Vector(marr(i)).dot(v)
//      }
//      Vector(values)
//    }

}
