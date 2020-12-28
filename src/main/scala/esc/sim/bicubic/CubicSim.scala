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
          val s0 = Random.nextInt(0xFFF + 0xFF) - 0x7F
          val s1 = Random.nextInt(0xFFF + 0xFF) - 0x7F
          val s2 = Random.nextInt(0xFFF + 0xFF) - 0x7F
          val s3 = Random.nextInt(0xFFF + 0xFF) - 0x7F
          val delta = Random.nextInt(0xFFF)

          dut.io.input.samples(0).raw #= s0
          dut.io.input.samples(1).raw #= s1
          dut.io.input.samples(2).raw #= s2
          dut.io.input.samples(3).raw #= s3
          dut.io.input.delta.raw #= delta
          dut.io.input.valid #= true
          dut.io.output.ready #= true
          dut.clockDomain.waitSampling()
          dut.io.input.valid #= false
          dut.clockDomain.waitSampling(11)

          val w0 = (s1 - s2) * 3 + s3 - s0
          val w1 = 2 * s0 - 5 * s1 + 4 * s2 - s3
          val w2 = s2 - s0
          val w3 = s1

          var result = ((w0 * delta) >> 12) + w1
          result = ((result * delta) >> 12) + w2
          result >>= 1
          result = ((result * delta) >> 12) + w3

          assert(dut.io.output.valid.toBoolean)
          assert(dut.io.output.payload.raw.toInt == result)
        }
      }
  }
}