package esc.bicubic

import spinal.core._
import spinal.lib._

case class CubicWeights() extends Component {
  val io = new Bundle {
    val samples = slave Flow(Vec(SFix(3 exp, 16 bits), 4))
    val weights = master Flow(Vec(SFix(3 exp, 16 bits), 4))
    val busy = out Bool
  }
  val busy = RegInit(False)
  io.busy := busy

  val samples = Reg(Vec(SFix(3 exp, 16 bits), 4))
  val weights = Reg(Vec(SFix(3 exp, 16 bits), 4))
  val weightsValid = RegNext(False) init(False)

  val stage = Reg(UInt(2 bits)) init(0)

  stage := stage + 1

  weights(2) := samples(2) - samples(0)
  weights(3) := samples(1)

  switch(stage) {
    is(0) {
      weights(0) := samples(1) - samples(2)
      weights(1) := ((samples(0) << 1) - (samples(1) << 2)).truncated
    }
    is(1) {
      weights(0) := ((weights(0) << 1) + weights(0)).truncated
      weights(1) := weights(1) - samples(1)
    }
    is(2) {
      weights(0) := weights(0) + samples(3)
      weights(1) := (weights(1) + (samples(2) << 2)).truncated
    }
    is(3) {
      weights(0) := weights(0) - samples(0)
      weights(1) := weights(1) - samples(3)
    }
  }

  when (stage === 3) {
    weightsValid := busy
    busy := False
  }

  when (io.samples.valid & !busy) {
    busy := True
    samples := io.samples.payload
    stage := 0
  }

  io.weights.payload := weights
  io.weights.valid := weightsValid
}

