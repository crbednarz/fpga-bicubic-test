package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._

object I2cMode extends SpinalEnum {
  val READ, WRITE = newElement()
}

case class I2cController(slaveAddress : BigInt, frequency: HertzNumber) extends Component {
  val io = new Bundle {
    val enable = in Bool
    val busy = out Bool
    val mode = in(I2cMode())

    val write = slave Stream(UInt(8 bits))
    val read = master Flow(UInt(8 bits))

    val scl = out Bool()
    val sda = master(ReadableOpenDrain(Bool()))
  }

  val timing = new Area {
    val divider = (ClockDomain.current.frequency.getValue / (frequency * 2)).toBigInt
    val counter = Reg(UInt(log2Up(divider) bits)) init(0)
    val clock = Reg(Bool()) init(True)

    val tickHigh = RegNext(False)
    val tickLow = RegNext(False)
    val tickMid = RegNext(False)
    val tickChange = tickMid & ~clock

    counter := counter - 1

    when (counter === 0) {
      counter := divider
      clock := ~clock

      tickHigh := clock === False
      tickLow := clock === True
    }

    when (counter === divider / 2) {
      tickMid := True
    }
  }

  val mode = Reg(I2cMode())

  val readFlowValid = RegNext(False)
  val readData = Reg(UInt(8 bits)) init(0)
  io.read.valid := readFlowValid
  io.read.payload := readData

  val writeStreamReady = RegNext(False)
  io.write.ready := writeStreamReady

  val clockOverride = Reg(Bool()) init(True)
  val clockOverrideValue = Reg(Bool()) init(True)
  io.scl := Mux(clockOverride, clockOverrideValue, timing.clock)

  val sdaWrite = Reg(Bool()) init(True)
  io.sda.write := sdaWrite

  val i2cFsm = new StateMachine {
    val writeData = Reg(UInt(8 bits)) init(0)
    val bitCounter = Reg(UInt(8 bits)) init(0)

    val readyState : State = new State with EntryPoint {
      whenIsActive {
        clockOverride := True
        clockOverrideValue := True
        when (io.enable) {
          mode := io.mode
          goto(startState)
        }
      }
    }

    val startState : State = new State {
      onEntry {
        sdaWrite := True
      }
      whenIsActive {
        when(mode === I2cMode.WRITE) {
          writeData := (slaveAddress << 1)
        } otherwise {
          writeData := (slaveAddress << 1) | 1
        }

        when (timing.tickMid && timing.clock) {
          sdaWrite := False
          clockOverride := False
        }

        when (timing.tickLow) {
          goto(writeState)
        }
      }
    }

    val writeState : State = new State {
      onEntry {
        bitCounter := 0
      }
      whenIsActive {
        when(timing.tickChange) {
          sdaWrite := writeData(7)
          writeData := writeData(6 downto 0) @@ False
          bitCounter := bitCounter + 1

          when(bitCounter === 8) {
            goto(writeAckState)
          }
        }
      }
    }
    val writeAckState : State = new State {
      onEntry {
        sdaWrite := True
      }
      whenIsActive {
        when (timing.tickLow) {
          when (io.enable) {
            when (mode === I2cMode.WRITE) {
              writeData := io.write.payload
              writeStreamReady := True
              goto(writeState)
            } otherwise {
              goto(readState)
            }
          } otherwise {
            goto(stopState)
          }
        }
      }
    }

    val readState : State = new State {
      onEntry {
        bitCounter := 0
      }
      whenIsActive {
        when (timing.tickChange) {
          sdaWrite := True
        }

        when (timing.tickHigh) {
          readData := readData(6 downto 0) @@ io.sda.read
          bitCounter := bitCounter + 1
        }
        when (timing.tickLow && bitCounter === 8) {
          readFlowValid := True
          goto(readAckState)
        }
      }
    }

    val readAckState : State = new State {
      whenIsActive {
        when (timing.tickChange) {
          sdaWrite := ~io.enable
        }

        when (timing.tickLow) {
          when (io.enable) {
            goto(readState)
          } otherwise {
            goto(stopState)
          }
        }
      }
    }

    val stopState : State = new State {
      onEntry {
        sdaWrite := False
      }
      whenIsActive {
        when (timing.tickChange) {
          clockOverride := True
          clockOverrideValue := True
        }
        when (timing.tickHigh) {
          sdaWrite := True
        }
        when (timing.tickLow) {
          goto(readyState)
        }
      }
    }

    io.busy := ~isActive(readyState)
  }
}
