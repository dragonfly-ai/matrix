package Jama.util

import scala.scalajs.js

@js.native
object Maths extends js.Object {
  /** sqrt(a^2 + b^2) without under/overflow. **/
  def hypot(a: Double, b: Double): Double = js.native
}
