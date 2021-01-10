package esc.sim.bicubic

import esc.Frame
import esc.bicubic._
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random


object BicubicUpscalerSim {
  class BicubicUpscalerDut(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int, sourceImage: Array[Array[Int]]) extends Component {
    val io = new Bundle {
      val enable = in Bool()

      val output = master Stream(SFix(3 exp, 16 bits))
    }

    def frameData =
      for (i <- 0 until sourceWidth * sourceHeight) yield U(sourceImage(i / sourceWidth)(i % sourceWidth))
    val frame = Frame(UInt(12 bits), sourceWidth, sourceHeight) init(frameData)

    frame.io.input.address := 0
    frame.io.input.valid := False
    frame.io.input.data := 0

    val bicubic = BicubicUpscaler(sourceWidth, sourceHeight, destWidth, destHeight)
    bicubic.io.enable := io.enable
    bicubic.io.source <> frame.io.output

    io.output <> bicubic.io.output
  }
  def main(args: Array[String]) {
    val sourceWidth = 5
    val sourceHeight = 5
    val destWidth = 22
    val destHeight = 22
    val xIncrement = ((sourceWidth / (destWidth - 1.0)) * 0x10000).toInt
    val yIncrement = ((sourceHeight / (destHeight - 1.0)) * 0x10000).toInt

    val sourceImage = Array.ofDim[Int](sourceHeight, sourceWidth)
    for (y <- 0 until sourceHeight) {
      for (x <- 0 until sourceWidth) {
        sourceImage(y)(x) = Random.nextInt(0xFFF)
      }
    }

    def clampX(x: Int) = Math.max(0, Math.min(sourceWidth - 1, x))
    def clampY(y: Int) = Math.max(0, Math.min(sourceHeight - 1, y))

    def bicubic(sourceX: Int, sourceY: Int): Int = {
      val xIndex = (sourceX / 0x10000) - 1
      val yIndex = (sourceY / 0x10000) - 1
      val xDelta = sourceX % 0x10000
      val yDelta = sourceY % 0x10000


      val values = new Array[Int](4)
      var i = 0
      for (x <- xIndex - 1 to xIndex + 2) {
        values(i) = BicubicUtil.calculateCubic(Array(
          sourceImage(clampY(yIndex - 1))(clampX(x)),
          sourceImage(clampY(yIndex + 0))(clampX(x)),
          sourceImage(clampY(yIndex + 1))(clampX(x)),
          sourceImage(clampY(yIndex + 2))(clampX(x))
        ), yDelta >> 4)
        i += 1
      }


      println(s"For $xIndex, $yIndex. Delta: $xDelta, $yDelta")
      for (y <- yIndex - 1 to yIndex + 2) {
        for (x <- xIndex - 1 to xIndex + 2) {
          val sample = sourceImage(clampY(y))(clampX(x))
          print(f"$sample%6d ")
        }
        println("")
      }
      println(values.mkString(" "))

      val result = BicubicUtil.calculateCubic(values, xDelta >> 4)
      println(s"Result: $result")
      result
    }

    SimConfig
      .withWave
      .doSim(new BicubicUpscalerDut(sourceWidth, sourceHeight, destWidth, destHeight, sourceImage)) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.output.ready #= true
        dut.clockDomain.waitSampling(2)
        dut.io.enable #= true
        dut.clockDomain.waitSampling()
        dut.io.enable #= false

        for (z <- 0 until 1) {
          var sourceY = 0x8000
          for (y <- 0 until destHeight) {
            var sourceX = 0x8000
            for (x <- 0 until destWidth) {
              while (!dut.io.output.valid.toBoolean) {
                dut.clockDomain.waitSampling(if (z > 0) 2 else 1)
              }
              val expected = bicubic(sourceX, sourceY)
              val actual = dut.io.output.payload.raw.toInt

              assert(actual == expected, s"For {$x, $y, $z} Expected: $expected Actual: $actual")

              sourceX += xIncrement
              if (z > 0) {
                dut.io.output.ready #= false
                dut.clockDomain.waitSampling(2)
                dut.io.output.ready #= true
              }
              dut.clockDomain.waitSampling()
            }
            sourceY += yIncrement
          }

          dut.io.enable #= true
          dut.clockDomain.waitSampling()
          dut.io.enable #= false
        }
      }
  }
}