package esc.bicubic

import spinal.core._
import spinal.lib._

case class ScaledIndex(sourceSize: Int, destSize: Int, sourceOffset: BigDecimal = 0.5) extends Bundle {
  val sourceIndex = UFix(log2Up(sourceSize) exp, -16 exp)
  val destIndex = UInt(log2Up(destSize + 1) bits)
  val sourceChanged = Bool()
  val end = Bool()

  def init(): ScaledIndex = {
    sourceIndex init(sourceOffset)
    destIndex init(0)
    sourceChanged init(True)
    end init(False)
    this
  }

  def deltaIncrement = UF((sourceSize - 1.0) / (destSize - 1.0), 0 exp, -16 exp)

  def start(): ScaledIndex = {
    val result = ScaledIndex(sourceSize, destSize, sourceOffset)
    result.sourceIndex := 0.5
    result.destIndex := 0
    result.sourceChanged := True
    result.end := False
    result
  }

  def next(): ScaledIndex = {
    val result = ScaledIndex(sourceSize, destSize, sourceOffset)
    val nextDelta = sourceIndex + deltaIncrement
    val sourceWillChange = sourceIndex.raw(16) =/= nextDelta.raw(16)
    val nextDestIndex = destIndex + 1
    result.sourceIndex := nextDelta
    result.destIndex := nextDestIndex

    when (end) {
      result.sourceIndex := sourceOffset
      result.destIndex := 0
    }

    result.sourceChanged := sourceWillChange
    result.end := nextDestIndex === destSize - 1
    result
  }
}
