package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import esc._


object Amg8833Sim {
  def main(args: Array[String]) {
    val spinalConfig = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(10 MHz))

    SimConfig
      .withWave
      .withConfig(spinalConfig)
      .doSim(new Amg8833()) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      var sda = true
      for(idx <- 0 to 30000){
        dut.io.sda.read #= sda
        if (dut.i2cCtrl.timing.tickChange.toBoolean) {
          sda = !sda

        }
        dut.clockDomain.waitSampling()
      }
    }
  }
}