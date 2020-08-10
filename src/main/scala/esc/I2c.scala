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

    counter := counter - 1

    when (counter === 0) {
      counter := divider
      clock := ~clock

      tickHigh := clock === False
      tickLow := clock === True
    }

    def reset = {
      counter := 0
      clock := True

    }
  }

  val mode = Reg(I2cMode())

  val fsm = new StateMachine {
    val clockEnabled = Reg(Bool()) init(False)
    val sdaWrite = Reg(Bool()) init(True)
    val writeStreamReady = RegNext(False)

    io.write.ready := writeStreamReady
    io.scl := timing.clock | ~clockEnabled
    io.sda.write := sdaWrite

    val writeData = Reg(UInt(8 bits)) init(0)
    val bitCounter = Reg(UInt(8 bits)) init(0)


    val readyState : State = new State with EntryPoint {
      whenIsActive {
        clockEnabled := False
        when (io.enable) {
          mode := io.mode
          goto(startState)
        }
      }
    }

    val startState : State = new State {
      whenIsActive {
        when (timing.tickHigh) {
          clockEnabled := True
          sdaWrite := False

          when (mode === I2cMode.WRITE) {
            writeData := (slaveAddress << 1)
            goto(writeState)
          } otherwise {
            writeData := (slaveAddress << 1) | 1
            goto(writeState)
          }
        }
      }
    }

    val writeState : State = new State {
      onEntry {
        bitCounter := 0
      }
      whenIsActive {
        when (timing.tickLow) {
          sdaWrite := writeData(7)
          writeData := (writeData << 1)(7 downto 0)
          bitCounter := bitCounter + 1

          when (bitCounter === 7) {
            goto(ackState)
          }
        }
      }

      val ackState : State = new State {
        onEntry {
          sdaWrite := True
        }
        whenIsActive {
          when (timing.tickLow) {
            when (io.enable) {
              writeData := io.write.payload
              writeStreamReady := True
              goto(writeState)
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
          when (timing.tickLow) {
            sdaWrite := True
            goto(readyState)
          }
        }
      }
    }

  }
}
