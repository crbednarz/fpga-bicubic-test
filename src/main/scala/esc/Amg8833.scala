package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._


case class Amg8833() extends Component {
  val io = new Bundle {
    val scl = out Bool()
    val sda = master(ReadableOpenDrain(Bool()))

  }

  val i2cEnable = Reg(Bool()) init(False)
  val i2cWriteValid = Reg(Bool()) init(False)
  val i2cWritePayload = Reg(UInt(8 bits))
  val i2cMode = Reg(I2cMode())

  val i2cCtrl = I2cController(0x69, 200000 Hz)
  i2cCtrl.io.enable := i2cEnable
  i2cCtrl.io.write.valid := i2cWriteValid
  i2cCtrl.io.write.payload := i2cWritePayload
  i2cCtrl.io.mode := i2cMode

  io.scl <> i2cCtrl.io.scl
  io.sda <> i2cCtrl.io.sda

  val initSequence = Mem(UInt(8 bits), initialContent = Array(
    // PCTRL -> Normal
    U(0),
    U(0),

    // Reset -> Initial Reset
    U(1),
    U(0x3F),

    // FPS -> 10 FPS
    U(2),
    U(0)
  ))

  val INIT_PACKET_COUNT = 3


  def bootFsm() = new StateMachine {
    // While is would probably be worth encoding the length of each command group
    // into the initSequence, we don't really need that flexibility. All commands
    // are two bytes: One for the register, one for the value.

    val commandIndex = Reg(UInt(2 bits)) init(0)

    val writeRegisterState : State = new State with EntryPoint {
      whenIsActive {
        i2cEnable := True
        i2cMode := I2cMode.WRITE
        i2cWriteValid := True
        i2cWritePayload := initSequence(commandIndex << 1)

        when (i2cCtrl.io.write.ready) {
          i2cWritePayload := initSequence((commandIndex << 1) | 1)
          goto(waitForRegisterState)
        }
      }
    }

    val waitForRegisterState : State = new State {
      whenIsActive {
        when (i2cCtrl.io.write.ready) {
          i2cEnable := False
          i2cWriteValid := False
        }
        when (~i2cCtrl.io.busy) {
          commandIndex := commandIndex + 1
          when (commandIndex === INIT_PACKET_COUNT - 1) {
            exit()
          } otherwise {
            goto(writeRegisterState)
          }
        }
      }
    }
  }

  val stateMachine = new StateMachine {
    val bytesRead = Reg(UInt(8 bits))

    val initState : State = new StateFsm(fsm=bootFsm()) with EntryPoint {
      onEntry {
        fsm.commandIndex := 0
      }
      whenCompleted {
        goto(beginRequestState)
      }
    }

    val beginRequestState : State = new State {
      whenIsActive {
        i2cEnable := True
        i2cMode := I2cMode.WRITE
        i2cWriteValid := True
        i2cWritePayload := 0x80

        when (i2cCtrl.io.write.ready) {
          i2cEnable := False
          i2cWriteValid := False

          goto(waitForRegisterState)
        }
      }
    }

    val waitForRegisterState : State = new State {
      whenIsActive {
        when (~i2cCtrl.io.busy) {
          goto(readState)
        }
      }
    }

    val readState : State = new State {
      onEntry {
        bytesRead := 0
        i2cEnable := True
        i2cMode := I2cMode.READ
      }
      whenIsActive {
        when (i2cCtrl.io.read.valid) {
          bytesRead := bytesRead + 1

          when (bytesRead === 16 * 2 - 1) {
            i2cEnable := False
            goto(waitForReadState)
          }
        }
      }
    }


    val waitForReadState : State = new State {
      whenIsActive {
        when (~i2cCtrl.io.busy) {
          goto(beginRequestState)
        }
      }
    }
  }
}
