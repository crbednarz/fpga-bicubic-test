package esc

import spinal.core._

case class SB_IO_TriState(pinType : String, pullUp : String) extends BlackBox {
  val io = new Bundle {
    val PACKAGE_PIN = inout(Analog(Bool))
    val OUTPUT_ENABLE = in Bool
    val D_OUT_0 = in Bool
    val D_IN_0 = out Bool
  }
  addGeneric("PIN_TYPE", B(pinType))
  addGeneric("PULLUP", B(pullUp))
  setDefinitionName("SB_IO")
  noIoPrefix()
}

case class TriStateIO() extends Component {
  val io = new Bundle {
    val pin = inout(Analog(Bool))
    val read = out Bool
    val write = in Bool
  }
  val buffer = SB_IO_TriState("101001", "0")
  buffer.io.OUTPUT_ENABLE <> ~io.write
  buffer.io.D_IN_0 <> io.read
  buffer.io.D_OUT_0 <> False
  buffer.io.PACKAGE_PIN <> io.pin
}