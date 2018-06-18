package ai.dragonfly.math

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

package object matrix {

  implicit def toJsArray(a: Array[Array[Double]]): js.Array[js.Array[Double]] = {
    val jsArr = js.Array[js.Array[Double]]()
    for (r <- a) {
      jsArr.push(r.toJSArray)
    }
    jsArr
  }

}
