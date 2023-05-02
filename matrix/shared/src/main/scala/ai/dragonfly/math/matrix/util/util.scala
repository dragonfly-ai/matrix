/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.matrix

import narr.*
import ai.dragonfly.math.vector.{Vec, *}
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

  extension (thisMatrix: Matrix) {
    def asVector[N <: Int]: Vec[N] = {
      if (thisMatrix.getColumnDimension() == 1 || thisMatrix.getRowDimension() == 1) {
        thisMatrix.getRowPackedCopy().asInstanceOf[Vec[N]]
      } else throw CannotExpressMatrixAsVector(thisMatrix)
    }
  }

  extension[N <: Int] (thisVector: Vec[N]) {
    inline def asRowMatrix: Matrix = new Matrix(thisVector.asInstanceOf[NArray[Double]], 1)
    inline def asColumnMatrix: Matrix = new Matrix(thisVector.asInstanceOf[NArray[Double]], thisVector.dimension)

  }


}
