/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.matrix.decomposition.QR
import ai.dragonfly.math.matrix.*

object LinearRegressionQR extends LinearRegression {

  override def estimateBeta(X: Matrix, Y: Matrix): Matrix = {
    val QRD: QR = QR(X)
    //    println(s"${X.dim} ${Y.dim}")
    QRD.solve(Y)
  }

}
