package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic.Rgb
import spinal.lib.io._


case class ColorScale() extends Component {
  val io = new Bundle {
    val input = slave Stream(SFix(3 exp, 16 bits))
    val output = master Stream(Rgb(8, 8, 8))
  }

  io.output.ready <> io.input.ready
  io.output.valid <> io.input.valid

  val color = Rgb(8, 8, 8)
  io.output.payload := color

  val scale = io.input.payload.raw(5 downto 0).asUInt << 2
  val invScale = ~scale

  switch (io.input.payload.raw(7 downto 6).asUInt) {
    is (U("b00")) {
      color.r := 0
      color.g := 0
      color.b := scale
    }
    is (U("b01")) {
      color.r := 0
      color.g := scale
      color.b := invScale
    }
    is (U("b10")) {
      color.r := scale
      color.g := invScale
      color.b := 0
    }
    is (U("b11")) {
      color.r := 0xFF
      color.g := scale
      color.b := scale
    }
  }
}
