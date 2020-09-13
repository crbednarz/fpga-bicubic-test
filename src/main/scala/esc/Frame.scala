package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._

case class FrameWrite[T <: Data](wordType: HardType[T], width: BigInt, height: BigInt) extends Bundle with IMasterSlave {
  def addressWidth = log2Up(width * height)

  val data: T = wordType()
  val address = UInt(log2Up(width * height) bits)
  val valid = Bool

  override def asMaster(): Unit = {
    out(data, address, valid)
  }
}


case class FrameRead[T <: Data](wordType: HardType[T], width: BigInt, height: BigInt) extends Bundle with IMasterSlave {
  def addressWidth = log2Up(width * height)

  val data: T = wordType()
  val address = UInt(log2Up(width * height) bits)

  override def asMaster(): Unit = {
    in(data)
    out(address)
  }
}

case class Frame[T <: Data](wordType: HardType[T], width: BigInt, height: BigInt) extends Component {
  def frameWriteType: HardType[FrameWrite[T]] = FrameWrite(wordType, width, height)
  def frameReadType: HardType[FrameRead[T]] = FrameRead(wordType, width, height)

  val io = new Bundle {
    val input = slave(frameWriteType())
    val output = slave(frameReadType())
  }

  val buffer = Mem(wordType, width * height)

  when (io.input.valid) {
    buffer.write(io.input.address, io.input.data)
  }

  io.output.data := buffer(io.output.address)
}
