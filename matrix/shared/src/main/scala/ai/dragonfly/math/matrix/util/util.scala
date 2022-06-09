package ai.dragonfly.math.matrix

import bridge.array.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*
import Jama.*

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
      for ( r <- values.indices ) {
        sb.append("\n")
        for ( c <- values(0).indices ){
          sb.append(s"${values(r)(c)}, ")
        }
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

//  def hypot(a: Double, b: Double): Double = {
//    var r = .0
//    if (Math.abs(a) > Math.abs(b)) {
//      r = b / a
//      r = Math.abs(a) * Math.sqrt(1 + r * r)
//    } else if (b != 0) {
//      r = a / b
//      r = Math.abs(b) * Math.sqrt(1 + r * r)
//    } else r = 0.0
//    r
//  }
}
