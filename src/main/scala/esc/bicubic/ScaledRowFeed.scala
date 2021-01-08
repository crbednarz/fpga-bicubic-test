package esc.bicubic

import spinal.core._
import spinal.lib._

/**
 * Converts a series of per-source-pixel values to per-destination-pixel values with a delta to represent the distance
 * between the last and next source pixel.
 */
case class ScaledRowFeed(sourceSize: Int, destSize: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream(SFix(3 exp, 16 bits))
    val output = master Stream(CubicInput())
  }

  val index = Reg(ScaledIndex(sourceSize + 1, destSize)) init
  val nextIndex = index.next()

  val outputValid = RegInit(False)

  val samples = Reg(Vec(SFix(3 exp, 16 bits), 4))

  val inputArea = new Area {
    val peeks = Reg(UInt(3 bits)) init(2)
    val pops = Reg(UInt(3 bits)) init(2)
    val hasPeeks = (peeks =/= 0)
    val hasPops = (pops =/= 0)
    val idle = !hasPops & !hasPeeks
    val willIdle = io.input.valid & (pops + peeks === 1)

    io.input.ready := !hasPeeks & hasPops

    when (io.input.valid) {
      when (hasPeeks | hasPops) {
        samples(3) := io.input.payload
        for (i <- 1 to 3) {
          samples(i - 1) := samples(i)
        }
      }

      when (hasPeeks) {
        peeks := peeks - 1
      } elsewhen (hasPops) {
        pops := pops - 1
      }
    }
  }

  io.output.valid := outputValid
  io.output.payload.delta.raw := index.sourceIndex.raw(15 downto 4).intoSInt.resized
  io.output.payload.samples := samples

  val canAssignOutput = io.output.ready || !outputValid
  when (canAssignOutput) {
    outputValid := False

    when (inputArea.idle) {
      index := nextIndex

      when (index.end) {
        inputArea.peeks := 2
        inputArea.pops := 2
      } otherwise {
        when (nextIndex.sourceChanged) {
          when (index.sourceIndex.toUInt === sourceSize - 3 || index.sourceIndex.toUInt === sourceSize - 2) {
            inputArea.peeks := 1
          } otherwise {
            inputArea.pops := 1
          }
        } otherwise {
          outputValid := True
        }
      }
    } elsewhen (inputArea.willIdle) {
      outputValid := True
    }
  }
}
