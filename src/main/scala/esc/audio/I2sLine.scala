package esc.audio

import spinal.core._
import spinal.lib._


case class I2sLineIn() extends Bundle with IMasterSlave {
  val mclk = Bool
  val lrck = Bool
  val sclk = Bool
  val sdout = Bool

  override def asMaster(): Unit = {
    out(mclk, lrck, sclk)
    in(sdout)
  }
}

case class I2sLineOut() extends Bundle with IMasterSlave {
  val mclk = Bool
  val lrck = Bool
  val sclk = Bool
  val sdin = Bool

  override def asMaster(): Unit = {
    out(mclk, lrck, sclk, sdin)
  }
}
