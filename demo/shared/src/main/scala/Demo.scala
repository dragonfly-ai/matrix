import ai.dragonfly.democrossy.{Demonstration, DivConsole, XApp}

import Console.*

object Demo extends XApp(DivConsole(style = "padding: 8px; overflow: scroll;")) with App {

  val allDemos: Array[Demonstration] = Array[Demonstration](
    DemoPCA,
    DemoLinearRegression,
    DemoEigenDecomposition
  )

  for (d <- allDemos) d.demonstrate
}
