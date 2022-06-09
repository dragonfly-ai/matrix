package Jama

import bridge.array.*

object EigenvalueDecomposition {

  // Symmetric Householder reduction to tridiagonal form.

  private def tred2(V: ARRAY[ARRAY[Double]], n: Int): EigenvalueDecomposition = {
    //  This is derived from the Algol procedures tred2 by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    val d: ARRAY[Double] = ARRAY.fill[Double](n)(0.0)
    val e: ARRAY[Double] = ARRAY.fill[Double](n)(0.0)

    for (j <- 0 until n) {
      d(j) = V(n - 1)(j)
    }

    // Householder reduction to tridiagonal form.

    for (i <- n - 1 until 0 by -1) { // Scale to avoid under/overflow.
      var scale = 0.0
      var h = 0.0
      for (k <- 0 until i) {
        scale = scale + Math.abs(d(k))
      }
      if (scale == 0.0) {
        e(i) = d(i - 1)
        for (j <- 0 until i) {
          d(j) = V(i - 1)(j)
          V(i)(j) = 0.0
          V(j)(i) = 0.0
        }
      }
      else { // Generate Householder vector.
        for (k <- 0 until i) {
          d(k) = d(k) / scale
          h += d(k) * d(k)
        }
        var f = d(i - 1)
        var g = Math.sqrt(h)
        if (f > 0) g = -g
        e(i) = scale * g
        h = h - f * g
        d(i - 1) = f - g
        for (j <- 0 until i) {
          e(j) = 0.0
        }
        // Apply similarity transformation to remaining columns.
        for (j <- 0 until i) {
          f = d(j)
          V(j)(i) = f
          g = e(j) + V(j)(j) * f
          for (k <- j + 1 to i - 1) {
            g += V(k)(j) * d(k)
            e(k) = e(k) + (V(k)(j) * f)
          }
          e(j) = g
        }
        f = 0.0
        for (j <- 0 until i) {
          e(j) = e(j) / h
          f += e(j) * d(j)
        }
        val hh = f / (h + h)
        for (j <- 0 until i) {
          e(j) = e(j) - (hh * d(j))
        }
        for (j <- 0 until i) {
          f = d(j)
          g = e(j)
          for (k <- j to i - 1) {
            V(k)(j) = V(k)(j) - (f * e(k) + g * d(k))
          }
          d(j) = V(i - 1)(j)
          V(i)(j) = 0.0
        }
      }
      d(i) = h
    }

    // Accumulate transformations.

    for (i <- 0 until n - 1) {
      V(n - 1)(i) = V(i)(i)
      V(i)(i) = 1.0
      val h = d(i + 1)
      if (h != 0.0) {
        for (k <- 0 to i) {
          d(k) = V(k)(i + 1) / h
        }
        for (j <- 0 to i) {
          var g = 0.0
          for (k <- 0 to i) {
            g += V(k)(i + 1) * V(k)(j)
          }
          for (k <- 0 to i) {
            V(k)(j) = V(k)(j) - (g * d(k))
          }
        }
      }
      for (k <- 0 to i) {
        V(k)(i + 1) = 0.0
      }
    }
    for (j <- 0 until n) {
      d(j) = V(n - 1)(j)
      V(n - 1)(j) = 0.0
    }
    V(n - 1)(n - 1) = 1.0
    e(0) = 0.0

    tql2(V, d, e, n)
  }

  // Symmetric tridiagonal QL algorithm.

  private def tql2(V: ARRAY[ARRAY[Double]], d: ARRAY[Double], e: ARRAY[Double], n: Int): EigenvalueDecomposition = {
    //  This is derived from the Algol procedures tql2, by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    for (i <- 1 until n) {
      e(i - 1) = e(i)
    }
    e(n - 1) = 0.0

    var f = 0.0
    var tst1 = 0.0
    val eps = Math.pow(2.0, -52.0)
    for (l <- 0 until n) { // Find small subdiagonal element
      tst1 = Math.max(tst1, Math.abs(d(l)) + Math.abs(e(l)))
      var m = l

      while (m < n && Math.abs(e(m)) > eps * tst1) m += 1

      // If m == l, d[l] is an eigenvalue,
      // otherwise, iterate.
      if (m > l) {
        var iter = 0
        var continue: Boolean = true
        while (continue) {
          iter = iter + 1 // (Could check iteration count here.)

          // Compute implicit shift
          var g = d(l)
          var p = (d(l + 1) - g) / (2.0 * e(l))
          var r = util.Maths.hypot(p, 1.0)
          if (p < 0) r = -r
          d(l) = e(l) / (p + r)
          d(l + 1) = e(l) * (p + r)
          val dl1 = d(l + 1)
          var h = g - d(l)
          for (i <- l + 2 until n) {
            d(i) = d(i) - h
          }
          f = f + h
          // Implicit QL transformation.
          p = d(m)
          var c = 1.0
          var c2 = c
          var c3 = c
          val el1 = e(l + 1)
          var s = 0.0
          var s2 = 0.0
          for (i <- m - 1 to l by -1) {
            c3 = c2
            c2 = c
            s2 = s
            g = c * e(i)
            h = c * p
            r = util.Maths.hypot(p, e(i))
            e(i + 1) = s * r
            s = e(i) / r
            c = p / r
            p = c * d(i) - s * g
            d(i + 1) = h + s * (c * g + s * d(i))
            // Accumulate transformation.
            for (k <- 0 until n) {
              h = V(k)(i + 1)
              V(k)(i + 1) = s * V(k)(i) + c * h
              V(k)(i) = c * V(k)(i) - s * h
            }
          }
          p = -s * s2 * c3 * el1 * e(l) / dl1
          e(l) = s * p
          d(l) = c * p
          // Check for convergence.
          continue = Math.abs(e(l)) > eps * tst1
        }
      }
      d(l) = d(l) + f
      e(l) = 0.0
    }

    // Sort eigenvalues and corresponding vectors.

    for (i <- 0 until n - 1) {
      var k = i
      var p = d(i)
      for (j <- i + 1 until n) {
        if (d(j) < p) {
          k = j
          p = d(j)
        }
      }
      if (k != i) {
        d(k) = d(i)
        d(i) = p
        for (j <- 0 until n) {
          p = V(j)(i)
          V(j)(i) = V(j)(k)
          V(j)(k) = p
        }
      }
    }

    new EigenvalueDecomposition(V, d, e, n)
  }

  private def orthes(V: ARRAY[ARRAY[Double]], n: Int): EigenvalueDecomposition = {
    //  This is derived from the Algol procedures orthes and ortran,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutines in EISPACK.

    val H: ARRAY[ARRAY[Double]] = Matrix(V).getArrayCopy()

    val ort: ARRAY[Double] = ARRAY.fill[Double](n)(0.0)

    val low = 0
    val high = n - 1

    for (m <- low + 1 to high - 1) { // Scale column.
      var scale = 0.0
      for (i <- m to high) {
        scale = scale + Math.abs(H(i)(m - 1))
      }
      if (scale != 0.0) { // Compute Householder transformation.
        var h = 0.0
        for (i <- high to m by -1) {
          ort(i) = H(i)(m - 1) / scale
          h += ort(i) * ort(i)
        }
        var g = Math.sqrt(h)
        if (ort(m) > 0) g = -g
        h = h - ort(m) * g
        ort(m) = ort(m) - g
        // Apply Householder similarity transformation
        // H = (I-u*u'/h)*H*(I-u*u')/h)
        for (j <- m until n) {
          var f = 0.0
          for (i <- high to m by -1) {
            f += ort(i) * H(i)(j)
          }
          f = f / h
          for (i <- m to high) {
            H(i)(j) = H(i)(j) - (f * ort(i))
          }
        }
        for (i <- 0 to high) {
          var f = 0.0
          for (j <- high to m by -1) {
            f += ort(j) * H(i)(j)
          }
          f = f / h
          for (j <- m to high) {
            H(i)(j) = H(i)(j) - (f * ort(j))
          }
        }
        ort(m) = scale * ort(m)
        H(m)(m - 1) = scale * g
      }
    }

    // Accumulate transformations (Algol's ortran).

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        V(i)(j) = if (i == j) 1.0
        else 0.0
      }
    }

    for (m <- high - 1 to low + 1 by -1) {
      if (H(m)(m - 1) != 0.0) {
        for (i <- m + 1 to high) {
          ort(i) = H(i)(m - 1)
        }
        for (j <- m to high) {
          var g = 0.0
          for (i <- m to high) {
            g += ort(i) * V(i)(j)
          }
          // Double division avoids possible underflow
          g = (g / ort(m)) / H(m)(m - 1)
          for (i <- m to high) {
            V(i)(j) = V(i)(j) + (g * ort(i))
          }
        }
      }
    }

    // Reduce Hessenberg to real Schur form.
    hqr2(V, H, n)
  }


  // Nonsymmetric reduction from Hessenberg to real Schur form.

  private def hqr2(V: ARRAY[ARRAY[Double]], H: ARRAY[ARRAY[Double]], dim: Int): EigenvalueDecomposition = {
    //  This is derived from the Algol procedure hqr2,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    // Initialize

    val d: ARRAY[Double] = ARRAY.fill[Double](dim)(0.0)
    val e: ARRAY[Double] = ARRAY.fill[Double](dim)(0.0)

    val nn: Int = dim
    var ni: Int = nn - 1
    val low: Int = 0
    val high: Int = nn - 1
    val eps: Double = Math.pow(2.0, -52.0)
    var exshift: Double = 0.0
    var p = 0.0
    var q = 0.0
    var r = 0.0
    var s = 0.0
    var z = 0.0
    var t = 0.0
    var w = 0.0
    var x = 0.0
    var y = 0.0


    def cdiv(xr: Double, xi: Double, yr: Double, yi: Double): (Double, Double) = {
      var r = 0.0
      var d = 0.0
      if (Math.abs(yr) > Math.abs(yi)) {
        r = yi / yr
        d = yr + r * yi
        (
          (xr + r * xi) / d, // temp._1 = ._1
          (xi - r * xr) / d // temp._2 = ._2
        )
      } else {
        r = yr / yi
        d = yi + r * yr
        (
          (r * xr + xi) / d, // temp._1 = ._1
          (r * xi - xr) / d // temp._2 = ._2
        )
      }
    }

    // Store roots isolated by balanc and compute matrix norm

    var norm = 0.0
    for (i <- 0 until nn) {
      if (i < low | i > high) {
        d(i) = H(i)(i)
        e(i) = 0.0
      }
      for (j <- Math.max(i - 1, 0) until nn) {
        norm = norm + Math.abs(H(i)(j))
      }
    }

    // Outer loop over eigenvalue index

    var iter = 0
    while (ni >= low) { // Look for single small sub-diagonal element

      var l = ni
      var continue: Boolean = true
      while (l > low && continue) {
        s = Math.abs(H(l - 1)(l - 1)) + Math.abs(H(l)(l))
        if (s == 0.0) s = norm
        if (Math.abs(H(l)(l - 1)) < eps * s) continue = false
        else l -= 1
      }

      // Check for convergence
      // One root found
      if (l == ni) {
        H(ni)(ni) = H(ni)(ni) + exshift
        d(ni) = H(ni)(ni)
        e(ni) = 0.0
        ni -= 1
        iter = 0
        // Two roots found
      } else if (l == ni - 1) {
        w = H(ni)(ni - 1) * H(ni - 1)(ni)
        p = (H(ni - 1)(ni - 1) - H(ni)(ni)) / 2.0
        q = p * p + w
        z = Math.sqrt(Math.abs(q))
        H(ni)(ni) = H(ni)(ni) + exshift
        H(ni - 1)(ni - 1) = H(ni - 1)(ni - 1) + exshift
        x = H(ni)(ni)
        // Real pair
        if (q >= 0) {
          if (p >= 0) z = p + z
          else z = p - z
          d(ni - 1) = x + z
          d(ni) = d(ni - 1)
          if (z != 0.0) d(ni) = x - w / z
          e(ni - 1) = 0.0
          e(ni) = 0.0
          x = H(ni)(ni - 1)
          s = Math.abs(x) + Math.abs(z)
          p = x / s
          q = z / s
          r = Math.sqrt(p * p + q * q)
          p = p / r
          q = q / r
          // Row modification
          for (j <- ni - 1 until nn) {
            z = H(ni - 1)(j)
            H(ni - 1)(j) = q * z + p * H(ni)(j)
            H(ni)(j) = q * H(ni)(j) - p * z
          }
          // Column modification
          for (i <- 0 to ni) {
            z = H(i)(ni - 1)
            H(i)(ni - 1) = q * z + p * H(i)(ni)
            H(i)(ni) = q * H(i)(ni) - p * z
          }
          // Accumulate transformations
          for (i <- low to high) {
            z = V(i)(ni - 1)
            V(i)(ni - 1) = q * z + p * V(i)(ni)
            V(i)(ni) = q * V(i)(ni) - p * z
          }
          // Complex pair
        } else {
          d(ni - 1) = x + p
          d(ni) = x + p
          e(ni - 1) = z
          e(ni) = -z
        }
        ni = ni - 2
        iter = 0
        // No convergence yet
      } else {
        // Form shift

        x = H(ni)(ni)
        y = 0.0
        w = 0.0
        if (l < ni) {
          y = H(ni - 1)(ni - 1)
          w = H(ni)(ni - 1) * H(ni - 1)(ni)
        }
        // Wilkinson's original ad hoc shift
        if (iter == 10) {
          exshift += x
          for (i <- low to ni) {
            H(i)(i) = H(i)(i) - x
          }
          s = Math.abs(H(ni)(ni - 1)) + Math.abs(H(ni - 1)(ni - 2))
          y = 0.75 * s
          x = y
          w = -0.4375 * s * s
        }

        // MATLAB's new ad hoc shift

        if (iter == 30) {
          s = (y - x) / 2.0
          s = s * s + w
          if (s > 0) {
            s = Math.sqrt(s)
            if (y < x) s = -s
            s = x - w / ((y - x) / 2.0 + s)
            for (i <- low to ni) {
              H(i)(i) = H(i)(i) - s
            }
            exshift += s
            w = 0.964
            y = w
            x = y
          }
        }
        iter = iter + 1 // (Could check iteration count here.)

        // Look for two consecutive small sub-diagonal elements
        var m: Int = ni - 2
        continue = true
        while (m >= l && continue) {

          z = H(m)(m)
          r = x - z
          s = y - z
          p = (r * s - w) / H(m + 1)(m) + H(m)(m + 1)
          q = H(m + 1)(m + 1) - z - r - s
          r = H(m + 2)(m + 1)
          s = Math.abs(p) + Math.abs(q) + Math.abs(r)
          p = p / s
          q = q / s
          r = r / s
          if (m == l || Math.abs(H(m)(m - 1)) * (Math.abs(q) + Math.abs(r)) < eps * (Math.abs(p) * (Math.abs(H(m - 1)(m - 1)) + Math.abs(z) + Math.abs(H(m + 1)(m + 1))))) {
            continue = false
          } else m -= 1
        }

        for (i <- m + 2 to ni) {
          H(i)(i - 2) = 0.0
          if (i > m + 2) {
            H(i)(i - 3) = 0.0
          }
        }

        // Double QR step involving rows l:ni and columns m:ni


        for (k <- m until ni) {
          continue = true
          val notlast: Boolean = k != ni - 1
          if (k != m) {
            p = H(k)(k - 1)
            q = H(k + 1)(k - 1)
            r = (if (notlast) H(k + 2)(k - 1) else 0.0)
            x = Math.abs(p) + Math.abs(q) + Math.abs(r)

            if (x == 0.0) {
              continue = false
            } else {
              p = p / x
              q = q / x
              r = r / x
            }
          }

          if (continue) {
            s = Math.sqrt(p * p + q * q + r * r)
            if (p < 0) {
              s = -s
            }
            if (s != 0) {
              if (k != m) {
                H(k)(k - 1) = -s * x
              } else {
                if (l != m) {
                  H(k)(k - 1) = -(H(k)(k - 1))
                }
              }
              p = p + s
              x = p / s
              y = q / s
              z = r / s
              q = q / p
              r = r / p
              // Row modification
              for (j <- k until nn) {
                p = H(k)(j) + q * H(k + 1)(j)
                if (notlast) {
                  p = p + r * H(k + 2)(j)
                  H(k + 2)(j) = H(k + 2)(j) - p * z
                }
                H(k)(j) = H(k)(j) - p * x
                H(k + 1)(j) = H(k + 1)(j) - p * y
              }
              // Column modification
              for (i <- 0 to Math.min(ni, k + 3)) {
                p = x * H(i)(k) + y * H(i)(k + 1)
                if (notlast) {
                  p = p + z * H(i)(k + 2)
                  H(i)(k + 2) = H(i)(k + 2) - p * r
                }
                H(i)(k) = H(i)(k) - p
                H(i)(k + 1) = H(i)(k + 1) - p * q
              }
              // Accumulate transformations
              for (i <- low to high) {
                p = x * V(i)(k) + y * V(i)(k + 1)
                if (notlast) {
                  p = p + z * V(i)(k + 2)
                  V(i)(k + 2) = V(i)(k + 2) - p * r
                }
                V(i)(k) = V(i)(k) - p
                V(i)(k + 1) = V(i)(k + 1) - p * q
              }
            } // (s != 0)
          } // if (continue)
        } // k loop
      } // check convergence
    } // while (ni >= low)

    // Backsubstitute to find vectors of upper triangular form

    if (norm == 0.0) return new EigenvalueDecomposition(V, d, e, ni)

    ni = nn - 1
    while (ni >= 0) {
      p = d(ni)
      q = e(ni)
      // Real vector
      if (q == 0) {
        var l = ni
        H(ni)(ni) = 1.0
        for (i <- ni - 1 to 0 by -1) {
          w = H(i)(i) - p
          r = 0.0
          for (j <- l to ni) {
            r = r + H(i)(j) * H(j)(ni)
          }
          if (e(i) < 0.0) {
            z = w
            s = r
          } else {
            l = i
            if (e(i) == 0.0) {
              if (w != 0.0) H(i)(ni) = -r / w
              else H(i)(ni) = -r / (eps * norm)
              // Solve real equations
            } else {
              x = H(i)(i + 1)
              y = H(i + 1)(i)
              q = (d(i) - p) * (d(i) - p) + e(i) * e(i)
              t = (x * s - z * r) / q
              H(i)(ni) = t
              if (Math.abs(x) > Math.abs(z)) H(i + 1)(ni) = (-r - w * t) / x
              else H(i + 1)(ni) = (-s - y * t) / z
            }
            // Overflow control
            t = Math.abs(H(i)(ni))
            if ((eps * t) * t > 1) for (j <- i to ni) {
              H(j)(ni) = H(j)(ni) / t
            }
          }
        }
        // Complex vector
      } else if (q < 0) {
        var l = ni - 1
        // Last vector component imaginary so matrix is triangular
        if (Math.abs(H(ni)(ni - 1)) > Math.abs(H(ni - 1)(ni))) {
          H(ni - 1)(ni - 1) = q / H(ni)(ni - 1)
          H(ni - 1)(ni) = -(H(ni)(ni) - p) / H(ni)(ni - 1)
        } else {
          val temp = cdiv(0.0, -H(ni - 1)(ni), H(ni - 1)(ni - 1) - p, q)
          H(ni - 1)(ni - 1) = temp._1
          H(ni - 1)(ni) = temp._2
        }

        H(ni)(ni - 1) = 0.0
        H(ni)(ni) = 1.0

        for (i <- ni - 2 to 0 by -1) {
          var ra = .0
          var sa = .0
          var vr = .0
          var vi = .0
          ra = 0.0
          sa = 0.0
          for (j <- l to ni) {
            ra = ra + H(i)(j) * H(j)(ni - 1)
            sa = sa + H(i)(j) * H(j)(ni)
          }
          w = H(i)(i) - p
          if (e(i) < 0.0) {
            z = w
            r = ra
            s = sa
          } else {
            l = i
            if (e(i) == 0) {
              val temp = cdiv(-ra, -sa, w, q)
              H(i)(ni - 1) = temp._1
              H(i)(ni) = temp._2
            } else { // Solve complex equations
              x = H(i)(i + 1)
              y = H(i + 1)(i)
              vr = (d(i) - p) * (d(i) - p) + e(i) * e(i) - q * q
              vi = (d(i) - p) * 2.0 * q
              if (vr == 0.0 & vi == 0.0) vr = eps * norm * (Math.abs(w) + Math.abs(q) + Math.abs(x) + Math.abs(y) + Math.abs(z))
              val temp = cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
              H(i)(ni - 1) = temp._1
              H(i)(ni) = temp._2
              if (Math.abs(x) > (Math.abs(z) + Math.abs(q))) {
                H(i + 1)(ni - 1) = (-ra - w * H(i)(ni - 1) + q * H(i)(ni)) / x
                H(i + 1)(ni) = (-sa - w * H(i)(ni) - q * H(i)(ni - 1)) / x
              } else {
                val temp = cdiv(-r - y * H(i)(ni - 1), -s - y * H(i)(ni), z, q)
                H(i + 1)(ni - 1) = temp._1
                H(i + 1)(ni) = temp._2
              }
            }
            t = Math.max(Math.abs(H(i)(ni - 1)), Math.abs(H(i)(ni)))
            if ((eps * t) * t > 1) for (j <- i to ni) {
              H(j)(ni - 1) = H(j)(ni - 1) / t
              H(j)(ni) = H(j)(ni) / t
            }
          }
        }
      }

      ni -= 1
    }
    // Vectors of isolated roots
    for (i <- 0 until nn) {
      if (i < low | i > high) for (j <- i until nn) {
        V(i)(j) = H(i)(j)
      }
    }

    // Back transformation to get eigenvectors of original matrix

    for (j <- nn - 1 to low by -1) {
      for (i <- low to high) {
        z = 0.0
        for (k <- low to Math.min(j, high)) {
          z = z + V(i)(k) * H(k)(j)
        }
        V(i)(j) = z
      }
    }


    new EigenvalueDecomposition(V, d, e, dim)
  }

  def apply(Arg: Matrix) = {

    val A = Arg.getArray()
    val n = Arg.getColumnDimension()

    var isSymmetric = true

    val V = ARRAY.tabulate[ARRAY[Double]](n)(
      (row: Int) => ARRAY.tabulate[Double](n)(
        (col: Int) => {
          if (isSymmetric) isSymmetric = A(row)(col) == A(col)(row)
          A(row)(col)
        }
      )
    )

    if (isSymmetric) tred2(V, n)  // Tridiagonalize
    else orthes(V, n)  // Reduce to Hessenberg form

  }

}

        /** Eigenvalues and eigenvectors of a real matrix.
         * <P>
         * If A is symmetric, then A = V*D*V' where the eigenvalue matrix D is
         * diagonal and the eigenvector matrix V is orthogonal.
         *I.e. A = V.times(D.times(V.transpose())) and
         *V.times(V.transpose()) equals the identity matrix.
         * <P>
         * If A is not symmetric, then the eigenvalue matrix D is block diagonal
         * with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues,
         * lambda + i*mu, in 2-by-2 blocks, [lambda, mu; -mu, lambda].  The
         * columns of V represent the eigenvectors in the sense that A*V = V*D,
         *i.e. A.times(V) equals V.times(D).  The matrix V may be badly
         * conditioned, or even singular, so the validity of the equation
         * A = V*D*inverse(V) depends upon V.cond().
         * */

        /** Check for symmetry, then construct the eigenvalue decomposition
         * Structure to access D and V.
         *
         * @param Arg Square matrix
         */

        class EigenvalueDecomposition(V:ARRAY[ARRAY[Double]], d:ARRAY[Double], e:ARRAY[Double], n:Int) {

          /** Return the eigenvector matrix
           *
           * @return V
           */
          def getV(): Matrix = Matrix(V)

          /** Return the real parts of the eigenvalues
           *
           * @return real(diag(D))
           */
          def getRealEigenvalues(): ARRAY[Double] = d

          /** Return the imaginary parts of the eigenvalues
           *
           * @return imag(diag(D))
           */
          def getImagEigenvalues(): ARRAY[Double] = e

          /** Return the block diagonal eigenvalue matrix
           *
           * @return D
           */
          def getD(): Matrix = {
            val X = new Matrix(n, n)
            val D = X.getArray()
            for (i <- 0 until n) {
              for (j <- 0 until n) {
                D(i)(j) = 0.0
              }
              D(i)(i) = d(i)
              if (e(i) > 0) D(i)(i + 1) = e(i)
              else if (e(i) < 0) D(i)(i - 1) = e(i)
            }
            X
          }
        }
