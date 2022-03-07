package ai.dragonfly.math.matrix.demo

import Jama.*
import ai.dragonfly.math.*
import ai.dragonfly.math.interval.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.data.StaticSupervisedData

import ai.dragonfly.math.example.Demonstrable

object Demo {

  val allDemos: Array[Demonstrable] = Array[Demonstrable](
    DemoPCA,
    DemoLinearRegression
  )

  import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}

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