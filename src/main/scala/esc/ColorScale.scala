package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic.Rgb
import spinal.lib.io._


case class ColorScale() extends Component {
  val io = new Bundle {
    val input = slave Stream(SInt(16 bits))
    val output = master Stream(Rgb(8, 8, 8))
  }

  val outputValid = RegInit(False)
  io.output.valid := outputValid

  val canOutput = (!outputValid | io.output.ready) & io.input.valid

  val inputReady = RegInit(False)
  io.input.ready := canOutput

  val outputValue = Reg(Rgb(8, 8, 8))
  io.output.payload := outputValue

  when (canOutput) {
    outputValid := True

    outputValue.r := io.input.payload(8 downto 1).asUInt
    outputValue.g := io.input.payload(8 downto 1).asUInt
    outputValue.b := io.input.payload(8 downto 1).asUInt
  }.elsewhen(io.output.ready) {
    outputValid := False
  }
}
