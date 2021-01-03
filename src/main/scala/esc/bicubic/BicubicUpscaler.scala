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
  val sampler = RowSampler(sourceWidth, sourceHeight)
  sampler.io.source <> io.source


  io.busy := False

}
