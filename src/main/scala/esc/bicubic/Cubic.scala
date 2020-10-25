package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._

case class CubicInput() extends Bundle {
  val samples = Vec(SInt(16 bits), 4)
  val delta = SInt(16 bits)
}

case class Cubic() extends Component {/*
  val io = new Bundle {
    val input = slave Stream(CubicInput())
    val output = master Stream(SInt(16 bits))
  }

  val input = Reg(CubicInput())
  val busy = RegInit(False)


  io.input.ready := busy

  when (!busy && io.input.valid) {
    input := io.input
    busy := True
    weights.valid := True
  }

  io.output.valid := !busy*/
}