package esc.bicubic

import spinal.core._
import spinal.lib._

/**
 * Basic wrapper around the a * b + c functionality of a DPS block.
 */
case class MultiplyAdd() extends Component {
  val io = new Bundle {
    val a = in SFix(3 exp, 16 bits)
    val b = in SFix(3 exp, 16 bits)
    val c = in SFix(3 exp, 16 bits)

    val output = out SFix(3 exp, 16 bits)
  }

  val output = SFix(3 exp, 16 bits)
  output := (io.a * io.b).truncated
  io.output := output + io.c
}

case class CubicInterpolate() extends Component {
  val io = new Bundle {
    val input = slave Stream(CubicParam())
    val output = master Stream(SFix(3 exp, 16 bits))
  }

  val busy = RegInit(False)
  val weights = Reg(Vec(SFix(3 exp, 16 bits), 4))
  val delta = Reg(SFix(3 exp, 16 bits))
  val stage = Reg(UInt(2 bits)) init(3)
  val outputValid = RegInit(False)

  val multiplyAdd = MultiplyAdd()
  multiplyAdd.io.a := delta
  multiplyAdd.io.b := weights(0)
  when (stage < 3) {
    weights(0) := multiplyAdd.io.output
  }

  io.input.ready := !busy

  switch (stage) {
    is (0) {
      multiplyAdd.io.c := weights(1)
    }
    is (1) {
      multiplyAdd.io.c := weights(2)
      weights(0) := (multiplyAdd.io.output >> 1).truncated
    }
    is (2) {
      multiplyAdd.io.c := weights(3)
      outputValid := True
    }
    is (3) {
      multiplyAdd.io.c := 0
    }
  }
  when (stage < 3) {
    stage := stage + 1
  }

  when (!busy && io.input.valid) {
    weights := io.input.samples
    delta := io.input.delta
    stage := 0
    busy := True
  }

  when (io.output.ready & outputValid) {
    outputValid := False
    busy := False
  }

  io.output.valid := outputValid
  io.output.payload := weights(0)
}
