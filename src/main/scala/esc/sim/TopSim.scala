package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random
import esc._
import spinal.lib.io._
import spinal.lib._


object TopSim {
  class Dut extends Component {

    val io = new Bundle {
      val scl = out Bool()
      val sda = master(ReadableOpenDrain(Bool()))
      val display = out(DisplayIo())
    }

    val camera = Amg8833()
    io.scl <> camera.io.scl
    io.sda <> camera.io.sda

    val frame = Frame(UInt(12 bits), 8, 8)
    frame.io.input <> camera.io.output

    val upscaler = Upscaler(256, 256)
    frame.io.output <> upscaler.io.source

    val converter = ColorScale()
    converter.io.input <> upscaler.io.upscaled

    val lcd = Display()
    io.display := lcd.io.display
    lcd.io.colorStream <> converter.io.output
  }
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz))

    SimConfig
      .withWave
      .withConfig(spinalConfig)
      .doSim(new Dut()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        var sda = true
        for(idx <- 0 to 30000){
          dut.io.sda.read #= sda
          if (dut.camera.i2cCtrl.timing.tickChange.toBoolean) {
            sda = !sda

          }
          dut.clockDomain.waitSampling()
        }
      }
  }
}