package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import esc.bicubic._


object CubicSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new Cubic()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 100) {
          val s0 = Random.nextInt(0xFFF)
          val s1 = Random.nextInt(0xFFF)
          val s2 = Random.nextInt(0xFFF)
          val s3 = Random.nextInt(0xFFF)
          val delta = Random.nextInt(0xFFF)

          dut.io.input.samples(0) #= s0
          dut.io.input.samples(1) #= s1
          dut.io.input.samples(2) #= s2
          dut.io.input.samples(3) #= s3
          dut.io.input.delta #= delta
          dut.io.input.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.input.valid #= false
          dut.clockDomain.waitSampling(10)

          val w0 = (s1 - s2) * 3 + s3 - s0
          val w1 = 2 * s0 - 5 * s1 + 4 * s2 - s3
          val w2 = s2 - s0
          val w3 = s1

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