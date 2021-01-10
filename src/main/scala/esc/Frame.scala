package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._

case class FrameWrite[T <: Data](wordType: HardType[T], width: Int, height: Int, channels: Int = 1) extends Bundle with IMasterSlave {
  def addressWidth = log2Up(width * height * channels)

  val data: T = wordType()
  val address = UInt(log2Up(width * height * channels) bits)
  val valid = Bool

  override def asMaster(): Unit = {
    out(data, address, valid)
  }
}


case class FrameRead[T <: Data](wordType: HardType[T], width: Int, height: Int, channels: Int = 1) extends Bundle with IMasterSlave {
  def addressWidth = log2Up(width * height * channels)

  val data: T = wordType()
  val address = UInt(log2Up(width * height * channels) bits)

  override def asMaster(): Unit = {
    in(data)
    out(address)
  }
}

case class Frame[T <: Data](wordType: HardType[T], width: Int, height: Int, channels: Int = 1) extends Component {
  def frameWriteType: HardType[FrameWrite[T]] = FrameWrite(wordType, width, height, channels)
  def frameReadType: HardType[FrameRead[T]] = FrameRead(wordType, width, height, channels)

  val io = new Bundle {
    val input = slave(frameWriteType())
    val output = slave(frameReadType())
  }

  val buffer = Mem(wordType, width * height * channels)

  def init(data: Seq[T]): this.type = {
    buffer.init(data)
    this
  }

  when (io.input.valid) {
    buffer.write(io.input.address, io.input.data)
  }

  io.output.data := buffer(io.output.address)
}

case class DoubleFrame[T <: Data](wordType: HardType[T], width: Int, height: Int, channels: Int = 1) extends Component {
  def frameWriteType: HardType[FrameWrite[T]] = FrameWrite(wordType, width, height, channels)
  def frameReadType: HardType[FrameRead[T]] = FrameRead(wordType, width, height, channels)

  val io = new Bundle {
    val input = slave(frameWriteType())
    val output = slave(frameReadType())
    val swap = in Bool()
  }

  val swapped = RegInit(False)

  when (io.swap.rise) {
    swapped := ~swapped
  }

  val ram = DualSPRAM()
  ram.io.swapped := swapped
  io.output.data := ram.io.read.dataOut.resize(12 bits).as(wordType)
  ram.io.read.address := io.output.address.resized
  ram.io.read.enable := True

  ram.io.write.dataIn := io.input.data.as(UInt(12 bits)).resized
  ram.io.write.address := io.input.address.resized
  ram.io.write.enable := io.input.valid
}
