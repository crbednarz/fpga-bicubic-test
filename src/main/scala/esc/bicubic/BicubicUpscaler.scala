package esc.bicubic
import esc.FrameRead
import spinal.core._
import spinal.lib._

case class BicubicUpscaler(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), sourceWidth, sourceHeight))
    val enable = in Bool()

    val output = master Stream(SFix(3 exp, 16 bits))
  }

  val sampler = SourceReader(sourceWidth, sourceHeight, destWidth, destHeight)
  sampler.io.source <> io.source
  sampler.io.enable <> io.enable

  val yCubic = Cubic()
  yCubic.io.input << sampler.io.output

  val scaledRow = RowScaler(sourceWidth, destWidth)
  scaledRow.io.input << yCubic.io.output

  val xCubic = Cubic()
  xCubic.io.input << scaledRow.io.output
  io.output <> xCubic.io.output
}
