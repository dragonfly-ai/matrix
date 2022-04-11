package ai.dragonfly.math.matrix

package object native {
  type MatrixValues = scala.scalajs.js.Array[ai.dragonfly.math.vector.VectorValues]
  type MatrixIndices = scala.scalajs.js.Array[Int]
  type MatrixValuesObject = scala.scalajs.js.Array.type
  val MatrixValues:MatrixValuesObject = scala.scalajs.js.Array
}