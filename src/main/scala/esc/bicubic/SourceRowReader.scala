package esc.bicubic

import esc.FrameRead
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/**
 * Reads four rows from a single channel image buffer.
 * As the goal here is to perform cubic interpolation, the read order is as follows:
 * [0] [4]
 * [1] [5]
 * [2] [6]
 * [3] [7]
 * And so on, heading left to right until the edge of the image is reached.
 * To help with cases where the target row is on edge of the source image, sourceIncMask can be used to control
 * whether the index is shifted down each time.
 * As an example, a mask of 110 would have the first four sample indices of: 0, 0, 1, 2.
 */
case class SourceRowReader(sourceWidth: Int, sourceHeight: Int) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), sourceWidth, sourceHeight))
    val sourceIndex = in UInt(log2Up(sourceWidth * sourceHeight) bits)
    val sourceIncMask = in Bits(3 bits)
    val enable = in Bool()

    val samples = master Stream(Vec(UInt(12 bits), 4))
    val busy = out Bool()
  }

  val xCount = Reg(UInt(log2Up(sourceWidth + 1) bits)) init(0)
  val index = Reg(UInt(log2Up(sourceWidth * sourceHeight) bits)) init(0)
  val baseIndex = Reg(UInt(log2Up(sourceWidth * sourceHeight) bits)) init(0)
  val incMask = Reg(Bits(4 bits)) init(0)
  val samples = Reg(Vec(UInt(12 bits), 4))
  val samplesValid = RegInit(False)
  val busy = RegInit(False)

  io.source.address := index
  io.samples.payload := samples
  io.samples.valid := samplesValid
  io.busy := busy

  val fsm = new StateMachine {
    val quadCounter = Reg(UInt(3 bits))
    val pixelCounter = Reg(UInt(log2Up(sourceWidth) bits))

    val idleState : State = new State with EntryPoint {
      onEntry {
        busy := False
      }
      whenIsActive {
        index := io.sourceIndex
        baseIndex := io.sourceIndex
        incMask := U"1" ## io.sourceIncMask
        xCount := 0

        when (io.enable) {
          busy := True
          goto(readQuadState)
        }
      }
    }

    val readQuadState : State = new State {
      onEntry {
        quadCounter := 0
        pixelCounter := 0
      }
      whenIsActive {
        samples(0) := samples(1)
        samples(1) := samples(2)
        samples(2) := samples(3)
        samples(3) := io.source.data

        when (incMask(0)) {
          index := index + sourceWidth
        }

        quadCounter := quadCounter + 1
        incMask := incMask.rotateRight(1)

        when (quadCounter === 3) {
          val nextBaseIndex = baseIndex + 1
          baseIndex := nextBaseIndex
          index := nextBaseIndex
          quadCounter := 0
          samplesValid := True
          xCount := xCount + 1
          goto(validState)
        }
      }
    }

    val validState : State = new State {
      whenIsActive {
        when (io.samples.ready) {
          samplesValid := False

          when (xCount === sourceWidth) {
            goto(idleState)
          } otherwise {
            goto(readQuadState)
          }
        }
      }
    }
  }
}