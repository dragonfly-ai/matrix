import ai.dragonfly.democrossy.*

import Console.*

object Demo extends XApp(NativeConsole(style = "padding: 8px; overflow: scroll;")) with App {

  val allDemos: Array[Demonstration] = Array[Demonstration](
    DemoPCA,
    DemoLinearRegression,
    DemoEigenDecomposition,
    DemoVectorInterop
  )

  for (d <- allDemos) d.demonstrate
}
