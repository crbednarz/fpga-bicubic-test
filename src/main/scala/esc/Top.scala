package esc

import esc.bicubic.BicubicUpscaler
import spinal.core._
import spinal.lib.Reverse
import spinal.lib.com.uart._

import sys.process._

class Top extends Component {
  val io = new Bundle {
    val led = new Bundle {
      val r = out Bool
      val g = out Bool
      val b = out Bool
    }
    val i2c = new Bundle {
      val sda = inout(Analog(Bool()))
      val scl = out Bool()
    }
    val display = out(DisplayIo())
  }
/*
  val clockControl = new Area {
    val pll = PLL30MHz()
    pll.io.clockIn := ClockDomain.current.readClockWire
    val domain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(30 MHz)
    )
    domain.clock := pll.io.clockOut
  }

  val core = new ClockingArea(clockControl.domain) {*/
  val core = new Area {
    io.led.r := True
    io.led.g := True
    io.led.b := True

    val sda = TriStateIO()
    sda.io.pin := io.i2c.sda

    val camera = Amg8833()
    io.i2c.scl <> camera.io.scl
    sda.io.write <> camera.io.sda.write
    sda.io.read <> camera.io.sda.read

    val frame = Frame(UInt(12 bits), 8, 8)
    frame.io.input <> camera.io.output

    val upscaler = BicubicUpscaler(8, 8, 256, 256)
    frame.io.output <> upscaler.io.source
    upscaler.io.enable := camera.io.frameComplete

    val converter = ColorScale()
    converter.io.input <> upscaler.io.output

    val lcd = Display()
    io.display := lcd.io.display
    lcd.io.colorStream <> converter.io.output
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