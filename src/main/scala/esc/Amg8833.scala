package esc

import spinal.core._
import spinal.lib._
import spinal.lib.io._

case class Amg8833() extends Component {
  val io = new Bundle {
    val scl = out Bool()
    val sda = master(ReadableOpenDrain(Bool()))

  }

  val i2cCtrl = I2cController(0x69, 200000 Hz)
  i2cCtrl.io.enable := True
  i2cCtrl.io.write.valid := True
  i2cCtrl.io.write.payload := 0x7
  i2cCtrl.io.mode := I2cMode.WRITE

  io.scl <> i2cCtrl.io.scl
  io.sda <> i2cCtrl.io.sda
}
