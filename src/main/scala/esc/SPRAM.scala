package esc

import spinal.core._

case class DualSPRAM() extends Component {
  val io = new Bundle {
    val swapped = in Bool
    val read = new Bundle {
      val address = in UInt(14 bits)
      val dataOut = out UInt(16 bits)
      val enable = in Bool
    }
    val write = new Bundle {
      val address = in UInt(14 bits)
      val dataIn = in UInt(16 bits)
      val enable = in Bool
    }
  }

  val spram = Array.fill(2)(SPRAM())

  // Swapped = Read on spram(0)
  spram(0).io.address := io.swapped ? io.read.address | io.write.address
  spram(0).io.writeEnable := ~io.swapped
  spram(0).io.enable := io.swapped ? io.read.enable | io.write.enable
  spram(0).io.dataIn := io.swapped ? U(0) | io.write.dataIn

  spram(1).io.address := io.swapped ? io.write.address | io.read.address
  spram(1).io.writeEnable := io.swapped
  spram(1).io.enable := io.swapped ? io.write.enable | io.read.enable
  spram(1).io.dataIn := io.swapped ? io.write.dataIn | U(0)

  io.read.dataOut := io.swapped ? spram(0).io.dataOut | spram(1).io.dataOut
}

case class SPRAM() extends Component {
  val io = new Bundle {
    val address = in UInt(14 bits)
    val writeEnable = in Bool
    val enable = in Bool
    val dataIn = in UInt(16 bits)
    val dataOut = out UInt(16 bits)
  }


  val ram = SB_SPRAM256KA()
  ram.io.ADDRESS <> io.address
  ram.io.DATAIN <> io.dataIn
  ram.io.DATAOUT <> io.dataOut
  ram.io.MASKWREN <> U"1111"
  ram.io.WREN <> io.writeEnable
  ram.io.CHIPSELECT <> io.enable
  ram.io.STANDBY <> False
  ram.io.SLEEP <> False
  ram.io.POWEROFF <> True
}

case class SB_SPRAM256KA() extends BlackBox {
  val io = new Bundle {
    val ADDRESS = in UInt(14 bits)
    val DATAIN = in UInt(16 bits)
    val DATAOUT = out UInt(16 bits)
    val MASKWREN = in UInt(4 bits)
    val WREN = in Bool
    val CHIPSELECT = in Bool
    val CLOCK = in Bool
    val STANDBY = in Bool
    val SLEEP = in Bool
    val POWEROFF = in Bool
  }
  mapClockDomain(clock=io.CLOCK)
  noIoPrefix()
}
