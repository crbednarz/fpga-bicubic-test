package esc.sim.bicubic

import esc.bicubic._
import spinal.core._
import spinal.core.sim._
import spinal.lib._


object RowScalerSim {
  class RowScalerDut(sourceSize: Int, destSize: Int) extends Component {
    val io = new Bundle {
      val output = master Stream(CubicInput())
    }

    val index = Reg(SInt(16 bits)) init(0)
    val valid = RegInit(True)

    val feed = RowScaler(sourceSize, destSize)
    feed.io.input.valid := valid
    feed.io.input.payload.raw := index

    when (feed.io.input.ready) {
      when (index === sourceSize - 1) {
        index := 0
      } otherwise {
        index := index + 1
      }
    }

    io.output <> feed.io.output
  }
  def main(args: Array[String]) {
    val sourceSize = 5
    val destSize = 22

    SimConfig
      .withWave
      .doSim(new RowScalerDut(sourceSize, destSize)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.output.ready #= false
        dut.clockDomain.waitSampling()
        dut.io.output.ready #= true

        val increment = sourceSize.toDouble / (destSize - 1.0)
        var index = 0
        for(y <- 0 to 3) {
          var sourceX = -0.5

          for(x <- 0 until destSize) {
            while (!dut.io.output.valid.toBoolean) {
              dut.clockDomain.waitSampling(if (y > 0) 2 else 1)
            }

            for (i <- 0 to 3) {
              val expected = Math.max(0, Math.min(sourceSize - 1, Math.floor(sourceX).toInt - 1 + i))
              val actual = dut.io.output.payload.samples(i).raw.toInt
              assert(actual == expected, s"For { $x, $y, $i } Expected: $expected, Actual: $actual")
            }

            val expectedDelta = sourceX - Math.floor(sourceX)
            val actualDelta = dut.io.output.delta.raw.toInt.toDouble / 0xFFF
            val margin = 0.05
            assert(actualDelta < expectedDelta + margin, s"For { $x, $y } Expected: $expectedDelta Actual: $actualDelta")
            assert(actualDelta > expectedDelta - margin, s"For { $x, $y } Expected: $expectedDelta Actual: $actualDelta")

            sourceX += increment
            index += 1
            if (y > 0) {
              dut.io.output.ready #= false
              dut.clockDomain.waitSampling(2)
              dut.io.output.ready #= true
            }
            dut.clockDomain.waitSampling()
          }
        }
      }
  }
}