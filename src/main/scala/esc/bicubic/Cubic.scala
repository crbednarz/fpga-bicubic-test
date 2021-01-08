package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._

case class CubicInput() extends Bundle {
  val samples = Vec(SFix(3 exp, 16 bits), 4)
  val delta = SFix(3 exp, 16 bits)
}

case class Cubic() extends Component {
  val io = new Bundle {
    val input = slave Stream(CubicInput())
    val output = master Stream(SFix(3 exp, 16 bits))
  }

  val weights = CubicWeights()
  val interpolate = CubicInterpolate()

  val delta = Reg(SFix(3 exp, 16 bits)) init(0)
  val busy = RegInit(False)

  val result = Reg(SFix(3 exp, 16 bits)) init(0)
  val resultValid = RegInit(False)
  val output = Reg(SFix(3 exp, 16 bits)) init(0)
  val outputValid = RegInit(False)

  interpolate.io.weights := weights.io.weights.payload
  interpolate.io.inputValid := weights.io.weights.valid
  interpolate.io.delta := delta
  weights.io.samples.payload := io.input.payload.samples
  weights.io.samples.valid := io.input.valid & !busy

  when (io.input.valid & !busy) {
    delta := io.input.payload.delta
    busy := True
    resultValid := False
  }

  io.input.ready := !busy

  when (interpolate.io.output.valid) {
    result := interpolate.io.output.payload
    resultValid := True
  }

  val canAssignOutput = io.output.ready || !outputValid
  when (canAssignOutput) {
    when (resultValid) {
      busy := False
      output := result
      outputValid := True
      resultValid := False
    } otherwise {
      outputValid := False
    }
  }

  io.output.payload := output
  io.output.valid := outputValid

}