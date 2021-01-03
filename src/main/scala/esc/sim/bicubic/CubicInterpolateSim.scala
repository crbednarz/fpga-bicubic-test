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

        for(idx <- 0 to 100) {
          val weights = BicubicUtil.randomWeights()
          val delta = BicubicUtil.randomDelta()

          for (i <- 0 to 3) {
            dut.io.weights(i).raw #= weights(i)
          }
          dut.io.delta.raw #= delta
          dut.io.inputValid #= true
          dut.clockDomain.waitSampling()
          dut.io.inputValid #= false
          dut.clockDomain.waitSampling(4)

          val expected = BicubicUtil.cubicInterpolate(weights, delta)

          assert(dut.io.output.valid.toBoolean)
          assert(dut.io.output.payload.raw.toInt == expected)
        }
      }
  }
}