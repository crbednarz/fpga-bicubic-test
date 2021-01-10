package esc.bicubic

import spinal.core._
import spinal.lib._


case class CubicWeights() extends Component {
  val io = new Bundle {
    val input = slave Stream(CubicParam())
    val output = master Stream(CubicParam())
  }

  val busy = RegInit(False)
  val samples = Reg(Vec(SFix(3 exp, 16 bits), 4))
  val stage = Reg(UInt(2 bits)) init(3)
  val output = Reg(CubicParam())
  val outputValid = RegInit(False)
  val weights = output.samples

  io.input.ready := !busy

  when (io.input.valid & !busy) {
    val inputSamples = io.input.payload.samples

    busy := True
    samples := inputSamples
    output.delta := io.input.payload.delta
    weights(0) := inputSamples(1) - inputSamples(2)
    weights(1) := ((inputSamples(0) << 1) - (inputSamples(1) << 2)).truncated
    stage := 0
  }

  when (stage < 3) {
    stage := stage + 1
  }

  weights(2) := samples(2) - samples(0)
  weights(3) := samples(1)

  switch(stage) {
    is(0) {
      weights(0) := ((weights(0) << 1) + weights(0)).truncated
      weights(1) := weights(1) - samples(1)
    }
    is(1) {
      weights(0) := weights(0) + samples(3)
      weights(1) := (weights(1) + (samples(2) << 2)).truncated
    }
    is(2) {
      weights(0) := weights(0) - samples(0)
      weights(1) := weights(1) - samples(3)
    }
  }

  when(stage === 2) {
    outputValid := True
  }

  when (io.output.ready & outputValid) {
    outputValid := False
    busy := False
  }

  io.output.payload := output
  io.output.valid := outputValid
}

