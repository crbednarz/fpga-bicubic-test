package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic.Rgb
import spinal.lib.io._


case class Upscaler(destWidth: BigInt, destHeight: BigInt) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), 8, 8))
    val upscaled = master Stream(UInt(12 bits))
  }

  val wordCounter = UInt(12 bits)

  val outputValid = RegInit(False)
  val outputPayload = Reg(UInt(12 bits)) init(0)
  io.upscaled.valid := outputValid
  io.upscaled.payload := outputPayload

  val outputAddress = Reg(UInt(16 bits)) init(0)
  io.source.address := outputAddress(15 downto 13) @@ outputAddress(7 downto 5)
  
  when (!outputValid | io.upscaled.ready) {
    outputPayload := io.source.data
    outputValid := True
    outputAddress := outputAddress + 1
  }
}
