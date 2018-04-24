# Welcome to matrix!

This Scala.js Matrix library publishes <a href="https://math.nist.gov/javanumerics/jama/">JAMA</a> for scala.js projects.

JVM only projects might better include JAMA directly from the <a href="https://mvnrepository.com/artifact/gov.nist.math/jama/1.0.3">maven repository</a> because on the JVM side of the cross-build, matrix simply includes the JAMA dependency.  However, from the JavaScript perspective, this library provides facades for a <a href="https://github.com/dragonfly-ai/JamaJS/blob/master/README.md">javascript version</a> of JAMA compiled by <a href="http://www.jsweet.org">Jsweet</a>.

# Exclusions

This implementation of JAMA excludes test and I/O functionality.
