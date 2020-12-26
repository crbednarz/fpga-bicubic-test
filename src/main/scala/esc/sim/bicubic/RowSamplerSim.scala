package esc.sim.bicubic

import esc.bicubic._
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random


object RowSamplerSim {
  class RowSamplerDut(width: Int, height: Int) extends Component {
    val io = new Bundle {
      val sourceIndex = in UInt(log2Up(width * height) bits)
      val sourceIncMask = in Bits(4 bits)
      val enable = in Bool

      val samples = master Stream(Vec(UInt(12 bits), 4))
      val busy = out Bool
    }
    val sampler = RowSampler(width, height)

    sampler.io.sourceIndex <> io.sourceIndex
    sampler.io.sourceIncMask <> io.sourceIncMask
    sampler.io.enable <> io.enable
    sampler.io.busy <> io.busy

    io.samples <> sampler.io.samples

    sampler.io.source.data := sampler.io.source.address.resized
  }
  def main(args: Array[String]) {
    val width = 10
    val height = 10
    val startIndex = width * 2

    SimConfig
      .withWave
      .doSim(new RowSamplerDut(width, height)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        dut.io.enable #= true
        dut.io.sourceIndex #= startIndex
        dut.io.sourceIncMask #= 0x3
        dut.clockDomain.waitSampling()
        dut.io.samples.ready #= false

        var index = startIndex

        for (idx <- 0 to (width - 1)) {
          while (!dut.io.samples.valid.toBoolean) {
            dut.clockDomain.waitSampling()
          }

          for (i <- 0 to 3) {
            val expected = index + Math.min(i, 1) * width
            assert(dut.io.samples.payload(i).toInt == expected)
          }

          dut.clockDomain.waitSampling()
          dut.io.samples.ready #= true
          dut.clockDomain.waitSampling()
          dut.io.samples.ready #= false
          dut.clockDomain.waitSampling()
          index += 1
        }

        assert(!dut.io.busy.toBoolean)
      }
  }
}