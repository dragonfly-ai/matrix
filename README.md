# Welcome to matrix!

This pure scala Matrix library provides matrix math capabilities to Scala and Scala.js.  Although it began as a 1:1 port of <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> for Scala 3 and Scala.js projects, it has expanded to include features that make matrix operations more comfortable in idiomatic Scala.

Features:
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

To use this library with SBT:

<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %%% "matrix" % "0.4.53"
</pre><br />

# JavaScript Optimization

Because matrix relies on <a href="https://github.com/dragonfly-ai/narr">NArr</a>, JavaScript environments store matrix data as:
```scala
var matrixArray:NArray[NArray[Double]]
```
which is equivalent to:
```scala
var matrixArray:js.Array[Float64Array]
```

# History
Past versions of this library JAMA from the <a href="https://mvnrepository.com/artifact/gov.nist.math/jama/1.0.3">maven repository</a> on the JVM side, and provided facades for a JavaScript version of JAMA ported through <a href="http://www.jsweet.org">Jsweet</a> and included through the <a href="https://scalacenter.github.io/scalajs-bundler/">scalajs-bundler</a> sbt plugin.  As scalajs-bundler has slipped into an unmaintained status, this library evolved into a pure scala port of JAMA and has begun to take its own shape.

# Exclusions

This implementation of JAMA excludes test and I/O functionality as well as some constructors that comments in the original JAMA library describe as dangerous and unnecessary.

# Verification

See the verification subproject of this repository to evaluate the fidelity of this port from Java to Scala.  Given the original JaMa implementation of hypot, these two matrix libraries produce identical output, however, modern Java includes a more advanced implementation of the hypot function and using it produces tiny discrepancies between Jama and this scala implementation.  