package esc.bicubic

import esc.FrameRead
import spinal.core._
import spinal.lib._

case class SourceReader(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), sourceWidth, sourceHeight))
    val output = master Stream(CubicInput())

    val enable = in Bool
  }

  val index = Reg(ScaledIndex(sourceHeight + 1, destHeight)) init()
  val sourceIndex = Reg(UInt(log2Up(sourceWidth * sourceHeight) bits)) init(0)
  val nextIndex = index.next()
  val enableReader = RegNext(False) init(False)

  val busy = RegInit(False)

  when (io.enable & !busy) {
    busy := True
    enableReader := True
    index := index.start()
    sourceIndex := 0
  }

  val mask = index.sourceIndex.toUInt.mux(mappings =
    0 -> B("b100"),
    1 -> B("b110"),
    sourceHeight - 1 -> B("b011"),
    sourceHeight -> B("b001"),
    default -> B("b111")
  )
  val shouldIncrement = index.sourceIndex.toUInt.mux(mappings =
    0 -> False,
    1 -> False,
    default -> True
  )

  val rowReader = SourceRowReader(sourceWidth, sourceHeight)
  io.source <> rowReader.io.source
  rowReader.io.sourceIndex := sourceIndex
  rowReader.io.sourceIncMask := mask
  rowReader.io.enable := enableReader
  // need to fix issue where y goes up instead of changing mask.

  when (rowReader.io.busy.fall & busy) {
    index := nextIndex

    when (nextIndex.sourceChanged & shouldIncrement) {
      sourceIndex := sourceIndex + sourceWidth
    }

    when (index.end) {
      busy := False
    } otherwise {
      enableReader := True
    }
  }

  io.output.payload.delta.raw := index.sourceIndex.raw(15 downto 4).intoSInt.resized
  for (i <- 0 to 3) {
    io.output.payload.samples(i).raw <> rowReader.io.samples.payload(i).intoSInt.resized
  }
  io.output.ready <> rowReader.io.samples.ready
  io.output.valid <> rowReader.io.samples.valid
}