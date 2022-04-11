package ai.dragonfly.math.matrix

package object native {
  type MatrixValues = Array[ai.dragonfly.math.vector.VectorValues]
  type MatrixIndices = Array[Int]
  type MatrixValuesObject = Array.type
  val MatrixValues:MatrixValuesObject = Array
}