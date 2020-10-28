package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._

case class CubicInput() extends Bundle {
  val samples = Vec(SInt(16 bits), 4)
  val delta = SInt(16 bits)
}

case class Cubic() extends Component {
  val io = new Bundle {
    val input = slave Stream(CubicInput())
    val output = master Flow(SInt(16 bits))
  }

  val weights = CubicWeights()
  val interpolate = CubicInterpolate()

  val delta = Reg(SInt(16 bits)) init(0)
  val busy = RegInit(False)

  interpolate.io.weights := weights.io.weights.payload
  interpolate.io.inputValid := weights.io.weights.valid
  interpolate.io.delta := delta
  weights.io.samples.payload := io.input.payload.samples
  weights.io.samples.valid := io.input.valid & !busy

  when (io.input.valid & !busy) {
    delta := io.input.payload.delta
    busy := True
  }

  val result = Reg(SInt(16 bits)) init(0)
  val outputValid = RegNext(False) init(False)

  when (interpolate.io.output.valid) {
    result := interpolate.io.output.payload
    outputValid := True
    busy := False
  }

  io.input.ready := !busy
  io.output.payload := result
  io.output.valid := outputValid
}