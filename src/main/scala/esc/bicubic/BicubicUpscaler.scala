package esc.bicubic
import esc.FrameRead
import spinal.core._
import spinal.lib._


case class BicubicUpscaler(sourceWidth: Int, sourceHeight: Int, destWidth: Int, destHeight: Int) extends Component {
  val io = new Bundle {
    val source = master(FrameRead(UInt(12 bits), sourceWidth, sourceHeight))
    val enable = in Bool()
    val busy = out Bool()

    val output = master Stream(SFix(3 exp, 16 bits))
  }

  val busy = RegInit(False)
  val activate = !busy & io.enable
  io.busy := busy

  when (activate) {
    busy := True
  }

  val yCounter = ScaledCounter(sourceHeight, destHeight)

  val sampler = SourceRowReader(sourceWidth, sourceHeight)
  sampler.io.source <> io.source
  sampler.io.sourceIncMask := 0x5
  sampler.io.sourceIndex := 0
  sampler.io.enable
  sampler.io.busy


  val yCubic = Cubic()
  yCubic.io.input.samples <> sampler.io.samples
  yCubic.io.input.delta := yCounter.io.delta


  val scaledRow = ScaledRowFeed(sourceWidth, destWidth)
  scaledRow.io.input << yCubic.io.output

  val xCubic = Cubic()
  xCubic.io.input << scaledRow.io.output
  io.output <> xCubic.io.output
}
