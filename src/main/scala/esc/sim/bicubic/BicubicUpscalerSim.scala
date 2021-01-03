package esc.sim.bicubic

import esc.Frame
import esc.bicubic._
import spinal.core._
import spinal.lib._
import spinal.core.sim._


object BicubicUpscalerSim {
  class BicubicUpscalerDut(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int) extends Component {
    val io = new Bundle {
      val enable = in Bool()
      val busy = out Bool()

      val output = master Stream(SFix(3 exp, 16 bits))
    }

    def frameData = for (i <- 0 until sourceWidth * sourceHeight) yield U(i)
    val frame = Frame(UInt(12 bits), sourceWidth, sourceHeight) init(frameData)

    frame.io.input.address := 0
    frame.io.input.valid := False
    frame.io.input.data := 0

    val bicubic = BicubicUpscaler(sourceWidth, sourceHeight, destWidth, destHeight)
    bicubic.io.enable := io.enable
    bicubic.io.source <> frame.io.output

    io.busy := bicubic.io.busy
    io.output <> bicubic.io.output
  }
  def main(args: Array[String]) {
    val sourceWidth = 10
    val sourceHeight = 10
    val destinationWidth = 100
    val destinationHeight = 100

    SimConfig
      .withWave
      .doSim(new BicubicUpscalerDut(sourceWidth, sourceHeight, destinationWidth, destinationHeight)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for (i <- 0 until 200) {
          dut.clockDomain.waitSampling()
        }
      }
  }
}