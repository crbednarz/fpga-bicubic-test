package esc.sim.bicubic

import esc.bicubic.BicubicUpscaler
import spinal.core._
import spinal.core.sim._


object BicubicSim {
  class BicubicDut extends Component {
    val io = new Bundle {
    }

    val upscaler = BicubicUpscaler(320, 320)
    upscaler.io.source.data := upscaler.io.source.address.resized
    upscaler.io.sourceValid := True
  }
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz))

    SimConfig
      .withWave
      .withConfig(spinalConfig)
      .doSim(new BicubicDut()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 30000){
          dut.clockDomain.waitSampling()
        }
      }
  }
}