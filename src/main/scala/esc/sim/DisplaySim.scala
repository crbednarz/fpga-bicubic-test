package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import esc._


object DisplaySim {
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(25 MHz))

    SimConfig
      .withWave
      .withConfig(spinalConfig)
      .doSim(new Display()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 30000){
          dut.clockDomain.waitSampling()
        }
      }
  }
}