
import bridge.array.ARRAY
import ai.dragonfly.math.example.Demonstrable
import ai.dragonfly.math.geometry.Line
import ai.dragonfly.math.matrix.ml.data.*
import ai.dragonfly.math.matrix.ml.unsupervised.dimreduction.PCA
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.visualization.ConsoleImage
import ai.dragonfly.math.{interval, visualization}

object DemoPCA extends Demonstrable {

  import interval.*
  import visualization.ConsoleImage

  def demo(implicit sb: StringBuilder = new StringBuilder()): StringBuilder = {

    // 2D shapes represented by centered 2D meshes of exactly 9 points each.
    val square = Vector(
      -1.000000, 1.000000, // point 1 1
      0.000000, 1.000000, // point 6 2
      1.000000, 1.000000, // point 2 3
      1.000000, 0.000000, // point 7 4

      1.000000, -1.000000, // point 4 5
      0.000000, -1.000000, // point 8 6
      -1.000000, -1.000000, // point 3 7
      -1.000000, 0.000000, // point 5 8
      0.000000, 0.000000 // point 9
    )

    val circle = Vector(
      -0.700000, 0.700000, // point 1 1
      0.000000, 1.000000, // point 6 2
      0.700000, 0.700000, // point 2 3
      1.000000, 0.000000, // point 7 4
      0.700000, -0.700000, // point 4 5
      0.000000, -1.000000, // point 8 6
      -0.700000, -0.700000, // point 3 7
      -1.000000, 0.000000, // point 5 8
      0.000000, 0.000000 // point 9
    )
    val almond = Vector(
      -0.488187, 0.800000, // point 1 1
      0.000000, 1.000000, // point 6 2
      0.503938, 0.800000, // point 2 3
      0.600000, 0.500000, // point 7 4
      0.450000, -0.200000, // point 4 5
      0.000000, -1.000000, // point 8 6
      -0.450000, -0.200000, // point 3 7
      -0.600000, 0.500000, // point 5 8
      0.000000, 0.500000 // point 9
    )
    val triangle = Vector(
      -1.000000, 1.000000, // point 1 1
      0.000000, 1.000000, // point 6 2
      1.000000, 1.000000, // point 2 3
      0.500000, 0.000000, // point 7 4
      0.100000, -0.800000, // point 4 5
      0.000000, -1.000000, // point 8 6
      -0.100000, -0.800000, // point 3 7
      -0.500000, 0.000000, // point 5 8
      0.000000, 0.000000 // point 9
    )
    val cross = Vector(
      -0.100000, 0.100000, // point 1 1
      0.000000, 1.000000, // point 6 2
      0.100000, 0.100000, // point 2 3
      1.000000, 0.000000, // point 7 4
      0.100000, -0.100000, // point 4 5
      0.000000, -1.000000, // point 8 6
      -0.100000, -0.100000, // point 3 7
      -1.000000, 0.000000, // point 5 8
      0.000000, 0.000000 // point 9
    )
    val x = Vector(
      -1.000000, 1.000000, // point 1 1
      0.000000, 0.100000, // point 6 2
      1.000000, 1.000000, // point 2 3
      0.100000, 0.000000, // point 7 4
      1.000000, -1.000000, // point 4 5
      0.000000, -0.100000, // point 8 6
      -1.000000, -1.000000, // point 3 7
      -0.100000, 0.000000, // point 5 8
      0.000000, 0.000000 // point 9
    )

    val vArr = ARRAY[Vector](square, circle, almond, triangle, cross, x)

    val cimg: ConsoleImage = ConsoleImage(50 * vArr.length, 50)

    sb.append("Sample Shapes:\n")
    for (i <- vArr.indices) {
      plotVectorOfShape2D(vArr(i), Vector2((i * 50) + 25, 25))(cimg)
    }

    sb.append(cimg)

    val pca = PCA(StaticUnsupervisedData(vArr))
    val reducer = pca.getReducer(3)

    sb.append(s"Mean Shape with μ = ${pca.mean}\n")
    sb.append(plotVectorOfShape2D(pca.mean, Vector2(25, 25))()).append("\n")

    for (bp <- pca.basisPairs) {
      if (bp.variance > 0.001) {
        var i = 0
        val cImg2: ConsoleImage = new ConsoleImage(350, 50)
        var s: Double = -3.0 * bp.variance
        while (s <= 3.0 * bp.variance) {
          plotVectorOfShape2D((bp.basisVector * s) + pca.mean, Vector2((i * 50) + 25, 25))(cImg2)
          s = s + bp.variance
          i = i + 1
        }
        sb.append(s"Singular Shape with σ = ${bp.variance} and Singular VSector: ${bp.basisVector}").append("\n")
        sb.append(s"Shape Variation from -3σ to 3σ (${-3.0 * bp.variance} to ${3.0 * bp.variance}):").append("\n")
        sb.append(cImg2).append("\n")
      }
    }

    sb.append(s"Dimensinoality reduction from ${reducer.domainDimension} to ${reducer.rangeDimension}:").append("\n")
    for (v <- vArr) {
      val cImg2: ConsoleImage = new ConsoleImage(100, 50)
      plotVectorOfShape2D(v, Vector2(25, 25))(cImg2)
      val reducedV = reducer(v)
      //plotVectorOfShape2D(reducedV, Vector2(75, 25))(cImg2)
      plotVectorOfShape2D(reducer.unapply(reducedV), Vector2(75, 25))(cImg2)
      sb.append(s"$v -> $reducedV").append("\n")
      sb.append(cImg2).append("\n")
    }

    sb
  }


  def name: String = "Principle Components Analysis"

  def plotVectorOfShape2D(sv: Vector, position: Vector2)(cimg: ConsoleImage = ConsoleImage(50, 50)): ConsoleImage = {
    def transform(x: Double, y: Double): Vector2 = Vector2((15 * x) + position.x, (15 * y) + position.y)

    def segment(i: Int, j: Int): Any = {
      Line.trace2D(
        transform(sv.values(i), sv.values(i + 1)),
        transform(sv.values(j), sv.values(j + 1)),
        (dX: Int, dY: Int) => {
          cimg.setPixel(dX, (cimg.height - 1) - dY, 2)
        }
      )
    }

    var i = 0
    while (i + 3 < sv.values.length) {
      segment(i, i + 2)
      segment(i, sv.values.length - 2)
      i = i + 2
    }
    segment(0, sv.values.length - 4)
    cimg
  }

}