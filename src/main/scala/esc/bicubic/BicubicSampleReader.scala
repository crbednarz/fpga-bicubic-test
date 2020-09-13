package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._


case class BicubicSampleReader(width: BigInt, height: BigInt) extends Component {
  val io = new Bundle {
    val enable = in Bool
    val source = master(FrameRead(UInt(12 bits), 8, 8))
    val output = master(Stream(Vec(UInt(12 bits), 4)))
  }

  // [N-1] [N+0] [N+1] [N+2]
  val FRONT_DELAY = 3

  val outputValid = RegInit(False)
  val active = RegInit(False)

  val canOutput = (!outputValid | io.output.ready) & active

  val address = Reg(UInt(io.source.addressWidth bits))
  var nextAddress = address
  val xIndex = Reg(UInt(log2Up(width + 8) bits))
  val yIndex = Reg(UInt(log2Up(height + 1) bits))

  val buffer = Reg(Vec(UInt(12 bits), 4))

  when (canOutput) {
    buffer(0) := buffer(1)
    buffer(1) := buffer(2)
    buffer(2) := buffer(3)
    buffer(3) := io.source.data

    xIndex := xIndex + 1

    outputValid := xIndex >= FRONT_DELAY

    val lineWrap = xIndex === width + FRONT_DELAY - 1
    val addressShouldMove = xIndex >= 2 & xIndex < width + 1

    when (lineWrap) {
      xIndex := 0
      nextAddress \= address + 1
      yIndex := yIndex + 1

      when (yIndex === height - 1) {
        active := False
        outputValid := False
      }
    }

    when (lineWrap | addressShouldMove) {
      nextAddress \= address + 1
    }
  }

  address := nextAddress

  when (io.enable & !active) {
    address := 0
    active := True
    xIndex := 0
    yIndex := 0
  }

  io.source.address := nextAddress
  io.output.payload := buffer
  io.output.valid := outputValid
}
