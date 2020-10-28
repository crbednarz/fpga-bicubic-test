package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import esc.bicubic._


object CubicInterpolateSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new CubicInterpolate()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 100) {
          val w0 = Random.nextInt(0xFFF << 1) - 0xFFF
          val w1 = Random.nextInt(0xFFF << 1) - 0xFFF
          val w2 = Random.nextInt(0xFFF << 1) - 0xFFF
          val w3 = Random.nextInt(0xFFF << 1) - 0xFFF
          val delta = Random.nextInt(0xFFF)

          dut.io.weights(0) #= w0
          dut.io.weights(1) #= w1
          dut.io.weights(2) #= w2
          dut.io.weights(3) #= w3
          dut.io.delta #= delta
          dut.io.inputValid #= true
          dut.clockDomain.waitSampling()
          dut.io.inputValid #= false
          dut.clockDomain.waitSampling(4)

          var result = ((w0 * delta) >> 12) + w1
          result = ((result * delta) >> 12) + w2
          result >>= 1
          result = ((result * delta) >> 12) + w3

          assert(dut.io.output.valid.toBoolean)
          assert(dut.io.output.payload.toInt == result)
        }
      }
  }
}