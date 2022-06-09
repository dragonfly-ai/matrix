import ai.dragonfly.math.example.Demonstrable

object Demo {

  val allDemos: Array[Demonstrable] = Array[Demonstrable](
    DemoPCA,
    DemoLinearRegression,
    DemoEigenValueDecomposition
  )

  import Console.*

  lazy val consolidateDemoOutput: String = {
    implicit val sb: StringBuilder = new StringBuilder()
    for (d <- allDemos) {
      sb.append(s"\n\n/* ${RESET}${GREEN}Begin ${d.name} Demonstration${RESET}*/\n")
      sb.append(s"${d.demo()}")
      sb.append(s"/* ${RESET}${RED}End ${d.name} Demonstration${RESET} */\n\n")
    }
    sb.toString()
  }


  def main(args: Array[String]): Unit = {
    println(s"${RESET}${GREEN}$consolidateDemoOutput${RESET}")
  }

}
