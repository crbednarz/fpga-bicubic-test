package esc.bicubic

import spinal.core._
import spinal.lib._

case class BicubicWeightCalculator() extends Component {
  val io = new Bundle {
    val input = slave Stream (Vec(UInt(12 bits), 4))
    val output = master Stream (Vec(SInt(16 bits), 4))
  }

  val outputValid = RegInit(False)
  io.output.valid := outputValid

  val busy = RegInit(False)

  val canConsumeInput = (!outputValid | io.output.ready) & !busy & io.input.valid
  io.input.ready := canConsumeInput

  val stage = Reg(UInt(2 bits))
  val samples = Reg(Vec(SInt(16 bits), 4))


  when(io.output.ready) {
    outputValid := False
  }

  when(busy) {
    stage := stage + 1
  }

  val weights = Reg(Vec(SInt(width = 16 bits), 4))
  io.output.payload := weights

  weights(2) := samples(2) - samples(0)
  weights(3) := samples(1)

  switch(stage) {
    is(0) {
      weights(0) := samples(1) - samples(2)
      weights(1) := ((samples(0) << 1) - (samples(1) << 2)).resized
    }
    is(1) {
      weights(0) := ((weights(0) << 1) + weights(0)).resized
      weights(1) := weights(1) - samples(1)
    }
    is(2) {
      weights(0) := weights(0) + samples(3)
      weights(1) := (weights(1) + (samples(2) << 2)).resized
    }
    is(3) {
      weights(0) := weights(0) - samples(0)
      weights(1) := weights(1) - samples(3)
      outputValid := True
      busy := False
    }
  }

  when(canConsumeInput) {
    samples(0) := S(io.input.payload(0)).resized
    samples(1) := S(io.input.payload(1)).resized
    samples(2) := S(io.input.payload(2)).resized
    samples(3) := S(io.input.payload(3)).resized

    stage := 0
    busy := True
  }
}
