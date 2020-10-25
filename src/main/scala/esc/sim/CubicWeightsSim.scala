package esc.sim


import spinal.core._
import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import esc.bicubic._


object CubicWeightsSim {
  def main(args: Array[String]) {
    SimConfig
      .withWave
      .doSim(new CubicWeights()) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for(idx <- 0 to 100) {
          val s0 = Random.nextInt(0xFFF)
          val s1 = Random.nextInt(0xFFF)
          val s2 = Random.nextInt(0xFFF)
          val s3 = Random.nextInt(0xFFF)

          dut.io.samples.payload(0) #= s0
          dut.io.samples.payload(1) #= s1
          dut.io.samples.payload(2) #= s2
          dut.io.samples.payload(3) #= s3
          dut.io.samples.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.samples.valid #= false
          dut.clockDomain.waitSampling(5)

          val w0 = dut.io.weights.payload(0).toInt
          val w1 = dut.io.weights.payload(1).toInt
          val w2 = dut.io.weights.payload(2).toInt
          val w3 = dut.io.weights.payload(3).toInt

          assert(w0 == (s1 - s2) * 3 + s3 - s0)
          assert(w1 == 2 * s0 - 5 * s1 + 4 * s2 - s3)
          assert(w2 == s2 - s0)
          assert(w3 == s1)
          assert(dut.io.weights.valid.toBoolean)
        }
      }
  }
}