# Welcome to matrix!

&nbsp;&nbsp;&nbsp;This pure scala Matrix library provides matrix math capabilities to Scala JVM, Scala Native, and Scala.js.&nbsp;&nbsp;You may <a href="https://dragonfly-ai.github.io/matrix/demo.html">try the demo</a> directly in the browser.

# Features
&nbsp;&nbsp;&nbsp;This matrix library differs most significantly from others like <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> and <a href="https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/org/apache/commons/math3/linear/">Apache Commons Math</a>, by providing compile time dimensionality checks.  Instead of encoding matrix row and column dimensions with method parameters or `Array[Double].length` values, this library relies on dependent types.  For example:

```scala
// create an 3 x 2 matrix of zeros.
val m:Matrix[3, 2] = Matrix.zeros[3, 2]
```

&nbsp;&nbsp;&nbsp;By encoding the matrix's row and column dimensions into its type, the compiler can prevent a whole category of runtime errors that arise from mismatched matrix dimensions:

```scala
// create an 3 x 2 matrix of zeros.
val m0:Matrix[3, 2] = Matrix.zeros[3, 2]
val m1:Matrix[2, 3] = Matrix.zeros[2, 3]

val m2:Matrix[3, 3] = m0 * m1
val m = m2 * m1 // compiler error!
```

&nbsp;&nbsp;&nbsp;Relatedly, many matrix operations like `determinant`, Cholesky decomposition, etc, only pertain to square matrices.  This library relies on type conditioned extension methods so that users simply cannot attempt to invoke these operations on rectangular matrices.  More specifically:

```scala
extension [MN <: Int] (m: Matrix[MN, MN])(using ValueOf[MN]) {
  def determinant: Double = LU[MN, MN](m).determinant
}
```

&nbsp;&nbsp;&nbsp;Instead of including a `determinant` method directly in the `Matrix` class, this extension method makes a `determinant` method available only for square matrices.  Trying to invoke the `determinant` method on a rectangular metrix, for which M != N, will yield a compiler error.

- Matrix math:
    + multiplication for Matrix * Matrix, Matrix * Vector, and Scalar * Matrix
    + element wise operations: add, subtract, multiply, and divide
    + sub-matrix, column, row, and element operations: get, set
    + determinant
    + transpose
    + inverse
    + norm operations: one, two, infinity, and Frobenius
    + decompositions: Cholesky, Eigen, LU, QR, and Singular Value
- In memory data sets: unsupervised and supervised
- Linear Regression based on both QR Decomposition and Singular Value Decomposition.
- Principal Components Analysis

# SBT

```scala
libraryDependencies += "ai.dragonfly" %%% "matrix" % "<LATEST_VERSION>"
```

# JavaScript Optimization

&nbsp;&nbsp;&nbsp;Because matrix relies on <a href="https://github.com/dragonfly-ai/narr">NArr</a>, JavaScript environments store matrix data as:
```scala
var matrixArray:NArray[NArray[Double]]
```
&nbsp;&nbsp;&nbsp;which is equivalent to:
```scala
var matrixArray:js.Array[Float64Array]
```
&nbsp;&nbsp;&nbsp;In JVM and Native environments, matrix data occupies normal scala `Array[Double]`.

# History
&nbsp;&nbsp;&nbsp;Although it began as a 1:1 port of <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> for Scala 3 and Scala.js projects, it has expanded to include features that make matrix operations more comfortable in idiomatic Scala. Past versions of this library JAMA from the <a href="https://mvnrepository.com/artifact/gov.nist.math/jama/1.0.3">maven repository</a> on the JVM side, and provided facades for a JavaScript version of JAMA ported through <a href="http://www.jsweet.org">Jsweet</a> and included through the <a href="https://scalacenter.github.io/scalajs-bundler/">scalajs-bundler</a> sbt plugin.  As scalajs-bundler has slipped into an unmaintained status, this library evolved into a pure scala port of JAMA and has begun to take its own shape.

# Exclusions
&nbsp;&nbsp;&nbsp;This implementation of JAMA excludes test and I/O functionality as well as some constructors that comments in the original JAMA library describe as dangerous and unnecessary.

# Verification
&nbsp;&nbsp;&nbsp;See the verification subproject of this repository to evaluate the fidelity of this port from Java to Scala.  Given the original JaMa implementation of hypot, these two matrix libraries produce identical output, however, modern Java includes a more advanced implementation of the hypot function and using it produces tiny discrepancies between Jama and this scala implementation.  