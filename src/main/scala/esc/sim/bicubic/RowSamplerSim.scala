package esc.sim.bicubic

import esc.Frame
import esc.bicubic._
import spinal.core._
import spinal.lib._
import spinal.core.sim._


object RowSamplerSim {
  class RowSamplerDut(width: Int, height: Int) extends Component {
    val io = new Bundle {
      val sourceIndex = in UInt(log2Up(width * height) bits)
      val sourceIncMask = in Bits(3 bits)
      val enable = in Bool

      val samples = master Stream(Vec(UInt(12 bits), 4))
      val busy = out Bool
    }

    def frameData = for (i <- 0 until width * height) yield U(i)

    val frame = Frame(UInt(12 bits), width, height) init(frameData)
    val sampler = RowSampler(width, height)

    frame.io.input.address := 0
    frame.io.input.valid := False
    frame.io.input.data := 0

    sampler.io.sourceIndex <> io.sourceIndex
    sampler.io.sourceIncMask <> io.sourceIncMask
    sampler.io.enable <> io.enable
    sampler.io.busy <> io.busy

    io.samples <> sampler.io.samples

    frame.io.output <> sampler.io.source
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
        dut.io.sourceIncMask #= 0x1
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