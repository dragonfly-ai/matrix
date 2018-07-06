package ai.dragonfly.math.matrix

import Jama.Matrix

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import ai.dragonfly.math.vector._

object MatrixUtils {

  implicit def toJsArray(a: Array[Array[Double]]): js.Array[js.Array[Double]] = {
    val jsArr = js.Array[js.Array[Double]]()
    for (r <- a) {
      jsArr.push(r.toJSArray)
    }
    jsArr
  }

  implicit def toArray(jsArr: js.Array[js.Array[Double]]): Array[Array[Double]] = {
    val arr = Array[Array[Double]]()
    for (i <- jsArr.indices) {
      arr(i) = jsArr(i).toArray
    }
    arr
  }

  implicit def toMatrix(a: Array[Double]): Matrix = {
    val marr = new Array[Array[Double]](a.length)
    for (i <- a.indices) {
      marr(i) = Array[Double](a(i))
    }
    new Matrix(marr)
  }

  implicit def toMatrix(v: Vector): Matrix = v.values

  implicit def toVector(m: Matrix): Vector = {
    if (m.getColumnDimension == 1) {
      new VectorN(m.getRowPackedCopy)
    } else throw MismatchedVectorDimensionsException(s"expected column dimension 1, found ${m.getColumnDimension}")
  }
}
