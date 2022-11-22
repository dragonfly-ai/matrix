package ai.dragonfly.math.matrix

import narr.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*

package object util {

  trait Dimensioned[M] {
    extension (m: M) def dim: String
    extension (m: M) def *(m1:Matrix):Matrix
  }

  given Dimensioned[Matrix] with
    extension (m: Matrix) def dim: String = s"dim(${m.getRowDimension()}x${m.getColumnDimension()})"
    extension (m: Matrix) def *(m1:Matrix):Matrix = m.times( m1 )
    extension (m: Matrix) def asString:String = {
      val values = m.getArray()
      val sb:StringBuilder = StringBuilder()
      var r:Int = 0; while (r < values.length ) {
        sb.append("\n")
        var c:Int = 0; while ( c < values(0).length ){
          sb.append(s"${values(r)(c)}, ")
          c = c + 1
        }
        r = r + 1
      }
      sb.toString()
    }


//  extension (a: Matrix) def *(b: Matrix): Matrix = a.times(b)

  extension (a: Matrix) def asVector: Vector = {
      if (a.getColumnDimension() == 1 || a.getRowDimension() == 1) {
        Vector(a.getRowPackedCopy())
      } else throw CannotExpressMatrixAsVector(a)
    }

  extension (a: Vector) def asRowMatrix: Matrix = new Matrix(a.values, 1)

  extension (a: Vector) def asColumnMatrix: Matrix = new Matrix(a.values, a.dimension)

}
