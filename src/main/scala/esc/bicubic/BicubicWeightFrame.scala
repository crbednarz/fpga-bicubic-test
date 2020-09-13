package esc.bicubic

import esc._
import spinal.core._
import spinal.lib._


case class BicubicWeightFrame(width: BigInt, height: BigInt) extends Component {
  val io = new Bundle {
    val sourceValid = in Bool
    val source = master(FrameRead(UInt(12 bits), 8, 8))
    val output = master(FrameRead(SInt(16 bits), 8, 8))
  }
  // The goal here is to generate 4 weights for each pixel. Having this
  // cached will save quite a bit of processing later on.
  // Since a single sysMEM block is 256x16, using a 16-bit signed integer
  // means we'll consumed exactly one block. (Assuming 8x8)

  val buffer = Mem(SInt(16 bits), width * height * 4)

  val isActive = RegInit(False)
  val sourceIndex = Reg(UInt(log2Up(width * height) bits)) init(0)

  val reader = BicubicSampleReader(width, height)
  io.source <> reader.io.source
  reader.io.enable := io.sourceValid

  val calculator = BicubicWeightCalculator()
  calculator.io.input << reader.io.output

  calculator.io.output.ready := True


  when (io.sourceValid & !isActive) {
    isActive := True
    sourceIndex := 0
  }

  when (isActive) {

  }
}