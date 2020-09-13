package esc.bicubic

import esc.FrameRead
import spinal.core._
import spinal.lib._


case class BicubicUpscaler(destWidth: BigInt, destHeight: BigInt) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), 8, 8))
    val sourceValid = in Bool
    val output = master Stream(UInt(12 bits))
  }

  val frame = BicubicWeightFrame(8, 8)
  frame.io.sourceValid := io.sourceValid
  io.source <> frame.io.source


  val wordCounter = UInt(12 bits)

  val outputValid = RegInit(False)
  val outputPayload = Reg(UInt(12 bits)) init(0)
  io.output.valid := outputValid
  io.output.payload := outputPayload

  when (!outputValid | io.output.ready) {
    outputPayload := io.source.data
    outputValid := True
  }
}
