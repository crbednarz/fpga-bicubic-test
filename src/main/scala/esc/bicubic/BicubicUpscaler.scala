package esc.bicubic

import esc.FrameRead
import spinal.core._
import spinal.lib._


case class BicubicUpscaler(destWidth: BigInt, destHeight: BigInt) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), 8, 8))
    val sourceValid = in Bool
    val output = master Stream(SInt(16 bits))
  }

  val frame = BicubicWeightFrame(8, 8)
  frame.io.sourceValid := io.sourceValid
  io.source <> frame.io.source

  val outputValid = RegInit(True)
  val outputPayload = Reg(SInt(16 bits)) init(0)

  io.output.valid := outputValid
  io.output.payload := outputPayload

  val canOutput = !outputValid | io.output.ready

  val xIndex = Reg(UInt(log2Up(destWidth) bits)) init(0)
  val yIndex = Reg(UInt(log2Up(destHeight) bits)) init(0)

  var nextX = UInt(xIndex.getWidth bits)
  var nextY = UInt(yIndex.getWidth bits)

  nextX := xIndex + 1
  nextY := yIndex

  val nextAddress = RegNext(frame.io.output.address)

  when (xIndex === destHeight - 1) {
    nextY := yIndex + 1
    nextX := 0

    when (yIndex === destHeight - 1) {
      nextY := 0
    }
  }

  when (canOutput) {
    xIndex := nextX
    yIndex := nextY
    outputPayload := frame.io.output.data
    outputValid := True
  }

  frame.io.output.address := nextY(7 downto 5) @@ nextX(7 downto 5) @@ U"11"
}
