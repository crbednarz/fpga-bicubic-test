package esc.audio

import spinal.core._
import spinal.lib._


object AudioChannel extends SpinalEnum {
  val left, right = newElement()
}


case class AudioSample() extends Bundle {
  val data = SInt(24 bits)
  val channel = AudioChannel()
}


case class I2sPairCtrl() extends Component {
  val io = new Bundle {
    val input = new Bundle {
      val line = master(I2sLineIn())
      val sample = master Flow(AudioSample())
    }
    val output = new Bundle {
      val line = master(I2sLineOut())
      val sample = slave Flow(AudioSample())
    }
  }

  val counter = Reg(UInt(10 bits)) init(0)
  counter := counter + 1

  val mclk = counter(0)
  val sclk = counter(1)
  val lrck = counter(7)

  val inputArea = new Area {
    val line = io.input.line
    val sample = io.input.sample

    val data = Reg(UInt(32 bits)) init(0)
    line.mclk := mclk
    line.lrck := lrck
    line.sclk := sclk
    when (line.sclk.rise()) {
      data := data(30 downto 0) @@ U(line.sdout)
    }

    sample.payload.data := S(data(30 downto 7))
    sample.payload.channel := line.sclk ? AudioChannel.left | AudioChannel.right
    val sampleValid = RegNext(False) init(False)
    sample.valid := sampleValid
    when (line.lrck.edge()) {
      sampleValid := True
    }
  }

  val outputArea = new Area {
    val line = io.output.line
    val sample = io.output.sample

    val leftSample = Reg(AudioSample())
    val rightSample = Reg(AudioSample())
    val data = Reg(UInt(32 bits)) init(0)
    line.mclk := mclk
    line.lrck := lrck
    line.sclk := sclk
    line.sdin := data(31)

    when (line.sclk.rise()) {
      data := (data << 1).resized
    }

    when (~counter(6 downto 0) === 0) {
      val nextSample = line.sclk ? rightSample | leftSample
      data := U"0" @@ U(nextSample.data) @@ U"0".resize(7)
    }

    when (sample.valid) {
      when (sample.payload.channel === AudioChannel.right) {
        rightSample := sample.payload
      } otherwise {
        leftSample := sample.payload
      }
    }

  }
}
