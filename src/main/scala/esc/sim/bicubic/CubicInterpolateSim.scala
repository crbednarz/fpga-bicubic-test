package esc.sim.bicubic

import esc.bicubic._
import spinal.core._
import spinal.core.sim._

import scala.util.Random


object CubicInterpolateSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new CubicInterpolate()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        dut.io.output.ready #= true
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling()

        for(idx <- 0 to 100) {
          val weights = BicubicUtil.randomWeights()
          val delta = BicubicUtil.randomDelta()

          for (i <- 0 to 3) {
            dut.io.input.payload.samples(i).raw #= weights(i)
          }
          dut.io.input.payload.delta.raw #= delta
          dut.io.input.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.input.valid #= false
          while (!dut.io.output.valid.toBoolean) {
            dut.clockDomain.waitSampling()
          }

          val expected = BicubicUtil.cubicInterpolate(weights, delta)
          val actual = dut.io.output.payload.raw.toInt
          assert(actual == expected, s"Expected $expected, got $actual")

          dut.clockDomain.waitSampling()
        }
      }
  }
}