package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._

case class CubicParam() extends Bundle {
  val samples = Vec(SFix(3 exp, 16 bits), 4)
  val delta = SFix(3 exp, 16 bits)
}

case class Cubic() extends Component {
  val io = new Bundle {
    val input = slave Stream(CubicParam())
    val output = master Stream(SFix(3 exp, 16 bits))
  }

  val weights = CubicWeights()
  weights.io.input <> io.input

  val interpolate = CubicInterpolate()
  interpolate.io.input <> weights.io.output
  io.output <> interpolate.io.output
}