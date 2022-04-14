# Welcome to matrix!

This Matrix library ports <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> for Scala 3 and Scala.js projects.

It also includes a growing number of convenience features for making common tasks more accessible:
  - Principal Components Analysis
  - Linear Regression: QR Decomposition and Singular Value Decomposition.

To use this library with SBT:

<pre>
resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/"
libraryDependencies += "ai.dragonfly.code" %%% "matrix" % "0.33.511"
</pre><br />

JVM only projects might better include JAMA directly from the <a href="https://mvnrepository.com/artifact/gov.nist.math/jama/1.0.3">maven repository</a> because on the JVM side of the cross-build, matrix simply includes the JAMA dependency.  However, from the JavaScript perspective, this library provides facades for a <a href="https://github.com/dragonfly-ai/JamaJS/blob/master/README.md">javascript version</a> of JAMA compiled by <a href="http://www.jsweet.org">Jsweet</a>.

# Exclusions

This implementation of JAMA excludes test and I/O functionality.
