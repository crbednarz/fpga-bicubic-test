package esc.sim.bicubic

import esc.bicubic._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object CubicSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new Cubic()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 100) {
          val samples = BicubicUtil.randomSamples()
          val delta = BicubicUtil.randomDelta()

          for (i <- 0 to 3) {
            dut.io.input.samples(i).raw #= samples(i)
          }
          dut.io.input.delta.raw #= delta
          dut.io.input.valid #= true
          dut.io.output.ready #= true
          dut.clockDomain.waitSampling()
          dut.io.input.valid #= false
          dut.clockDomain.waitSampling(11)

          val expected = BicubicUtil.calculateCubic(samples, delta)
          assert(dut.io.output.valid.toBoolean)
          assert(dut.io.output.payload.raw.toInt == expected)
        }
      }
  }
}