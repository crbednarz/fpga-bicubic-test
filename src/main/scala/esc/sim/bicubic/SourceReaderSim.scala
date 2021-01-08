package esc.sim.bicubic

import esc.Frame
import esc.bicubic._
import spinal.core._
import spinal.lib._
import spinal.core.sim._


object SourceReaderSim {
  class SourceReaderDut(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int) extends Component {
    val io = new Bundle {
      val output = master Stream(CubicInput())
      val enable = in Bool()
    }

    def frameData = for (i <- 0 until sourceWidth * sourceHeight) yield U(i)

    val frame = Frame(UInt(12 bits), sourceWidth, sourceHeight) init(frameData)
    val reader = SourceReader(sourceWidth, sourceHeight, destWidth, destHeight)
    reader.io.enable <> io.enable

    frame.io.input.address := 0
    frame.io.input.valid := False
    frame.io.input.data := 0

    io.output <> reader.io.output
    frame.io.output <> reader.io.source

  }
  def main(args: Array[String]) {
    val sourceWidth = 5
    val sourceHeight = 5
    val destWidth = 22
    val destHeight = 22
    val xIncrement = sourceWidth.toDouble / (destWidth - 1.0)
    val yIncrement = sourceHeight.toDouble / (destHeight - 1.0)

    SimConfig
      .withWave
      .doSim(new SourceReaderDut(sourceWidth, sourceHeight, destWidth, destHeight)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.output.ready #= true
        dut.clockDomain.waitSampling(2)
        dut.io.enable #= true

        for (z <- 0 until 3) {
          var sourceY = -0.5
          for (y <- 0 until destHeight) {

            for (x <- 0 until sourceWidth) {
              while (!dut.io.output.valid.toBoolean) {
                dut.clockDomain.waitSampling(1)
              }


              for (i <- 0 to 3) {
                val expectedY = Math.max(0, Math.min(sourceHeight - 1, Math.floor(sourceY).toInt - 1 + i))
                val expected = expectedY * sourceWidth + x
                val actual = dut.io.output.payload.samples(i).raw.toInt
                assert(actual == expected, s"For { $x, $y, $z, $i } Expected: $expected, Actual: $actual")
              }

              val expectedDelta = sourceY - Math.floor(sourceY)
              val actualDelta = dut.io.output.payload.delta.raw.toInt.toDouble / 0xFFF
              val margin = 0.05
              assert(actualDelta < expectedDelta + margin, s"For { $x, $y, $z } Expected: $expectedDelta Actual: $actualDelta")
              assert(actualDelta > expectedDelta - margin, s"For { $x, $y, $z } Expected: $expectedDelta Actual: $actualDelta")

              dut.clockDomain.waitSampling()
            }

            sourceY += yIncrement
          }
        }
      }
  }
}