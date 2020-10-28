package esc.bicubic

import spinal.core._
import spinal.lib._

/**
 * Basic wrapper around the a * b + c functionality of a DPS block.
 */
case class MultiplyAdd() extends Component {
  val io = new Bundle {
    val a = in SInt(16 bits)
    val b = in SInt(16 bits)
    val c = in SInt(16 bits)

    val output = out SInt(16 bits)
  }

  io.output := (io.a * io.b + (io.c << 12))(27 downto 12)
}

case class CubicInterpolate() extends Component {
  val io = new Bundle {
    val weights = in Vec(SInt(16 bits), 4)
    val delta = in SInt(16 bits)
    val inputValid = in Bool

    val busy = out Bool
    val output = master Flow(SInt(16 bits))
  }

  val weights = Reg(Vec(SInt(16 bits), 4))
  val delta = Reg(SInt(16 bits))

  val multiplyAdd = MultiplyAdd()
  multiplyAdd.io.a := delta
  multiplyAdd.io.b := weights(0)
  weights(0) := multiplyAdd.io.output

  val stage = Reg(UInt(2 bits)) init(0)

  switch (stage) {
    is (0) {
      multiplyAdd.io.c := weights(1)
    }
    is (1) {
      multiplyAdd.io.c := weights(2)
      weights(0) := (multiplyAdd.io.output >> 1).resized
    }
    is (2) {
      multiplyAdd.io.c := weights(3)
    }
    is (3) {
      multiplyAdd.io.c := 0
    }
  }
  stage := stage + 1

  val busy = RegInit(False)
  io.busy := busy

  val outputValid = RegNext(False) init(False)
  when (stage === 2) {
    outputValid := busy
    busy := False
  }

  when (!busy && io.inputValid) {
    weights := io.weights
    stage := 0
    delta := io.delta
    busy := True
  }


  io.output.valid := outputValid
  io.output.payload := weights(0)
}


object ProjectConfig extends SpinalConfig(
  defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
  defaultClockDomainFrequency = FixedFrequency(12 MHz))

object CubicInterpolateMain {
  def main(args: Array[String]) {
    ProjectConfig.generateVerilog(new CubicInterpolate).printPruned()
  }
}