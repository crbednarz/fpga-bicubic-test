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

  val delta = Reg(UFix(1 exp, 16 bits)) init(0.5)
  val deltaIncrement = UF(sourceSize.toDouble / (destSize - 1.0), 1 exp, 16 bits)
  val destIndex = Reg(UInt(log2Up(destSize + 1) bits)) init(0)
  val sourceIndex = Reg(UInt(log2Up(sourceSize + 1) bits)) init(0)
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

  val outputDelta = Reg(UFix(1 exp, 16 bits)) init(0)
  io.output.valid := outputValid
  io.output.payload.delta.raw := delta.raw(14 downto 3).intoSInt.resized
  io.output.payload.samples := samples

  val tempDelta = UFix(1 exp, 16 bits)
  tempDelta := delta + deltaIncrement
  val nextDelta = UFix(1 exp, 16 bits)
  nextDelta.raw := tempDelta.raw(14 downto 0).resized

  val sourceWillChange = tempDelta.raw.msb

  val outputIndex = Reg(SInt(log2Up(sourceSize + 1) + 1 bits)) init(0)

  val canAssignOutput = io.output.ready || !outputValid
  when (canAssignOutput) {
    outputValid := False

    when (inputArea.idle) {
      outputDelta := delta
      outputIndex := sourceIndex.intoSInt - 1

      when (destIndex === destSize - 1) {
        destIndex := 0
        sourceIndex := 0
        delta := 0.5
        inputArea.peeks := 2
        inputArea.pops := 2
      } otherwise {
        destIndex := destIndex + 1
        delta := nextDelta

        when (sourceWillChange) {
          sourceIndex := sourceIndex + 1
          when (sourceIndex === sourceSize - 3 || sourceIndex === sourceSize - 2) {
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
