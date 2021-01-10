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
        dut.io.output.ready #= true
        dut.io.input.valid #= false

        dut.clockDomain.waitSampling()
        for(idx <- 0 to 100) {
          val samples = BicubicUtil.randomSamples()
          val delta = BicubicUtil.randomDelta()

          dut.io.input.delta.raw #= delta
          for (i <- 0 to 3) {
            dut.io.input.payload.samples(i).raw #= samples(i)
          }
          dut.io.input.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.input.valid #= false
          while (!dut.io.output.valid.toBoolean) {
            dut.clockDomain.waitSampling()
          }

          val expectedWeights = BicubicUtil.cubicWeights(samples)
          for (i <- 0 to 3) {
            val actualWeight = dut.io.output.payload.samples(i).raw.toInt
            val expected = expectedWeights(i)
            assert(actualWeight == expected, s"Expected weight $expected got $actualWeight for $i")
          }
          val actualDelta = dut.io.output.delta.raw.toInt
          assert(actualDelta == delta, s"Expected delta $delta, got $actualDelta")
          dut.clockDomain.waitSampling()

        }
      }
  }
}