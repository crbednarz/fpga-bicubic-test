package esc

import esc.audio._
import spinal.core._
import spinal.lib.com.uart._
import spinal.lib._
import spinal.lib.com.i2c._
import spinal.lib.io.InOutWrapper

import sys.process._

class Top extends Component {
  val io = new Bundle {
    val led = new Bundle {
      val r = out Bool
      val g = out Bool
      val b = out Bool
    }
  }

  val clockControl = new Area {
    val pll = PLL25MHz()
    pll.io.clockIn := ClockDomain.current.readClockWire
    val domain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(25 MHz)
    )
    domain.clock := pll.io.clockOut
  }

  val core = new ClockingArea(clockControl.domain) {
    val counter = Reg(UInt(32 bits))

    counter := counter + 1

    io.led.r := counter(24)
    io.led.g := counter(25)
    io.led.b := counter(28)

  }

}

object ProjectConfig extends SpinalConfig(
  defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
  defaultClockDomainFrequency = FixedFrequency(12 MHz))

object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    ProjectConfig.generateVerilog(new Top)
    "build.bat" !
  }
}