package esc.sim.bicubic

import esc.bicubic._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object CubicWeightsSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new CubicWeights()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 100) {
          val samples = BicubicUtil.randomSamples()

          for (i <- 0 to 3) {
            dut.io.samples.payload(i).raw #= samples(i)
          }
          dut.io.samples.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.samples.valid #= false
          dut.clockDomain.waitSampling(5)

          val expected = BicubicUtil.cubicWeights(samples)
          for (i <- 0 to 3) {
            assert(dut.io.weights.payload(i).raw.toInt == expected(i))
          }
          assert(dut.io.weights.valid.toBoolean)
        }
      }
  }
}