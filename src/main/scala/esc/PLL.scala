package esc

import spinal.core._


case class SB_PLL40_PAD(divr : String, divf : String, divq : String) extends BlackBox {
  val io = new Bundle {
    val PACKAGEPIN = in Bool
    val PLLOUTCORE = out Bool
    val RESETB = in Bool
    val BYPASS = in Bool
  }
  addGeneric("FEEDBACK_PATH", "SIMPLE")
  addGeneric("DIVR", B(divr))
  addGeneric("DIVF", B(divf))
  addGeneric("DIVQ", B(divq))
  addGeneric("FILTER_RANGE", B"001")
  noIoPrefix()
}

case class PLL25MHz() extends Component {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  val pll = SB_PLL40_PAD(
    divr = "0000",
    divf = "1000010",
    divq = "101")
  pll.io.RESETB := True
  pll.io.BYPASS := False
  pll.io.PACKAGEPIN <> io.clockIn
  pll.io.PLLOUTCORE <> io.clockOut
}

case class PLL30MHz() extends Component {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  val pll = SB_PLL40_PAD(
    divr = "0000",
    divf = "1001111",
    divq = "101")
  pll.io.RESETB := True
  pll.io.BYPASS := False
  pll.io.PACKAGEPIN <> io.clockIn
  pll.io.PLLOUTCORE <> io.clockOut
}

case class PLL50MHz() extends Component {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  val pll = SB_PLL40_PAD(
    divr = "0000",
    divf = "1000010",
    divq = "100")
  pll.io.RESETB := True
  pll.io.BYPASS := False
  pll.io.PACKAGEPIN <> io.clockIn
  pll.io.PLLOUTCORE <> io.clockOut
}

case class PLL60MHz() extends Component {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  val pll = SB_PLL40_PAD(
    divr = "0000",
    divf = "1001111",
    divq = "100")
  pll.io.RESETB := True
  pll.io.BYPASS := False
  pll.io.PACKAGEPIN <> io.clockIn
  pll.io.PLLOUTCORE <> io.clockOut
}

case class PLL90MHz() extends Component {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  val pll = SB_PLL40_PAD(
    divr = "0000",
    divf = "1001111",
    divq = "011")
  pll.io.RESETB := True
  pll.io.BYPASS := False
  pll.io.PACKAGEPIN <> io.clockIn
  pll.io.PLLOUTCORE <> io.clockOut
}