package Jama.util

object Maths {
  /** sqrt(a^2 + b^2) without under/overflow. **/

  def hypot(a: Double, b: Double): Double = {
    var r = .0
    if (Math.abs(a) > Math.abs(b)) {
      r = b / a
      r = Math.abs(a) * Math.sqrt(1 + r * r)
    } else if (b != 0) {
      r = a / b
      r = Math.abs(b) * Math.sqrt(1 + r * r)
    } else r = 0.0
    r
  }
}
