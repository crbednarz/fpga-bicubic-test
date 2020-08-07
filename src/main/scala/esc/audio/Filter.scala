package esc.audio

import spinal.core._
import spinal.lib._

case class ChannelHistory(size : Int) extends Component {
  val io = new Bundle {
    val store = slave Flow(SInt(24 bits))
    val sampleStream = master Stream(SInt(24 bits))
  }
  val history = Mem(SInt(24 bits), size)
  val writeIndex = Reg(UInt(history.addressWidth bits)) init(0)
  val readIndex = Reg(UInt(history.addressWidth bits)) init(0)

  val sample = Reg(SInt(24 bits)) init(0)
  io.sampleStream.valid := True
  io.sampleStream.payload <> sample

  when (io.sampleStream.ready) {
    val nextReadIndex = (readIndex === 0) ? U(size - 1) | (readIndex - 1)
    sample := history.readSync(nextReadIndex)
    readIndex := nextReadIndex
  }

  when (io.store.valid) {
    history(writeIndex) := io.store.payload
    writeIndex := (writeIndex === size - 1) ? U(0) | (writeIndex + 1)

    sample := io.store.payload
    readIndex := writeIndex
  }
}

case class FilterKernel(rawFilter : Seq[Int]) extends Component {
  val io = new Bundle {
    val filterStream = master Stream(SInt(24 bits))
    val reset = in Bool
    val endOfList = out Bool
  }
  val filterList = Mem(SInt(24 bits), initialContent = rawFilter.map(x => S(x, 24 bits)))
  val index = Reg(UInt(filterList.addressWidth bits)) init(0)
  io.endOfList := (index === rawFilter.length - 1)

  val filter = Reg(SInt(24 bits)) init(0)
  io.filterStream.valid := True
  io.filterStream.payload <> filter

  when (io.filterStream.ready) {
    val nextIndex = (index === rawFilter.length - 1) ? U(0) | (index + 1)
    index := nextIndex
    filter := filterList(nextIndex)
  }

  when (io.reset) {
    filter := S(rawFilter(0), 24 bits)
    index := 0
  }
}

case class TestFilter() extends Component {
  val io = new Bundle {
    val input = slave Flow(AudioSample())
    val output = master Flow(AudioSample())
  }
  val rawFilter = Array(
    559,
    1263,
    2115,
    3118,
    4272,
    5579,
    7039,
    8653,
    10421,
    12340,
    14408,
    16625,
    18986,
    21489,
    24128,
    26900,
    29799,
    32819,
    35954,
    39197,
    42541,
    45978,
    49500,
    53098,
    56763,
    60486,
    64258,
    68068,
    71906,
    75763,
    79627,
    83488,
    87335,
    91159,
    94947,
    98690,
    102376,
    105996,
    109539,
    112996,
    116355,
    119609,
    122748,
    125762,
    128644,
    131386,
    133980,
    136418,
    138696,
    140806,
    142744,
    144504,
    146082,
    147475,
    148680,
    149695,
    150517,
    150517,
    149695,
    148680,
    147475,
    146082,
    144504,
    142744,
    140806,
    138696,
    136418,
    133980,
    131386,
    128644,
    125762,
    122748,
    119609,
    116355,
    112996,
    109539,
    105996,
    102376,
    98690,
    94947,
    91159,
    87335,
    83488,
    79627,
    75763,
    71906,
    68068,
    64258,
    60486,
    56763,
    53098,
    49500,
    45978,
    42541,
    39197,
    35954,
    32819,
    29799,
    26900,
    24128,
    21489,
    18986,
    16625,
    14408,
    12340,
    10421,
    8653,
    7039,
    5579,
    4272,
    3118,
    2115,
    1263,
    559
  )

  val filterKernel = FilterKernel(rawFilter)
  val leftHistory = ChannelHistory(rawFilter.length)
  val rightHistory = ChannelHistory(rawFilter.length)

  val channel = Reg(AudioChannel())
  val processingSample = RegInit(False)

  val outSample = Reg(SInt(24 bits)) init(0)

  val inputArea = new Area {
    filterKernel.io.reset := io.input.valid
    leftHistory.io.store.valid := io.input.valid && (io.input.channel === AudioChannel.left)
    rightHistory.io.store.valid := io.input.valid && (io.input.channel === AudioChannel.right)
    leftHistory.io.store.payload := io.input.data
    rightHistory.io.store.payload := io.input.data

    when (io.input.valid) {
      channel := io.input.channel
      processingSample := True
      outSample := 0
    }
  }

  val processingArea = new Area {
    filterKernel.io.filterStream.ready := processingSample
    val filter = RegNext(filterKernel.io.filterStream.payload) init(0)
    val isLeftChannel = (channel === AudioChannel.left)
    leftHistory.io.sampleStream.ready := isLeftChannel
    rightHistory.io.sampleStream.ready := !isLeftChannel
    val sample = RegNext(isLeftChannel ? leftHistory.io.sampleStream.payload | rightHistory.io.sampleStream.payload) init(0)

    when (RegNext(processingSample) init(False)) {
      outSample := outSample + ((sample * filter) >> 23)(23 downto 0)
    }
  }

  val outputArea = new Area {
    val outValid = RegNext(False) init(False)
    io.output.payload.channel := channel
    io.output.valid := outValid
    io.output.payload.data := outSample

    when (processingSample && filterKernel.io.endOfList) {
      processingSample := False
    }

    when ((RegNext(processingSample) init(False)) && !processingSample) {
      outValid := True
    }
  }
}
