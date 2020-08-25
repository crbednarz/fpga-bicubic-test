package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io._

case class DisplayIo() extends Bundle {
  val data = UInt(8 bits)
  val write = Bool()
  val cd = Bool()
  val cs = Bool()
}


object MessageMode extends SpinalEnum {
  val COMMAND, DATA = newElement()
}

case class DisplayMessage() extends Bundle {
  val data = UInt(8 bits)
  val mode = MessageMode()
  val hasDelay = Bool()
}

object DataMessage {
  def apply(data: BigInt, hasDelay: Bool = False): DisplayMessage = {
    val message = DisplayMessage()
    message.data := data
    message.mode := MessageMode.DATA
    message.hasDelay := hasDelay
    message
  }
}


object CommandMessage {
  def apply(data: BigInt, hasDelay: Bool = False): DisplayMessage = {
    val message = DisplayMessage()
    message.data := data
    message.mode := MessageMode.COMMAND
    message.hasDelay := hasDelay
    message
  }
}

case class DisplayInitializer() extends Component
{
  val io = new Bundle {
    val output = master Stream(DisplayMessage())
    val last = out Bool
  }

  val CMD_SWRESET = 0x1
  val CMD_SETEXTC = 0xB9
  val CMD_SETRGB = 0xB3
  val CMD_SETVCOM = 0xB6
  val CMD_SETOSC = 0xB0
  val CMD_SETPANEL = 0xCC
  val CMD_SETPOWER = 0xB1
  val CMD_SETSTBA = 0xC0
  val CMD_SETCYC = 0xB4
  val CMD_SETGAMMA = 0xE0
  val CMD_COLMOD = 0x3A
  val CMD_MADCTL = 0x36
  val CMD_TEON = 0x35
  val CMD_TEARLINE = 0x44
  val CMD_SLPOUT = 0x11
  val CMD_DISPON = 0x29
  val CMD_CASET = 0x2A
  val CMD_PASET = 0x2B


  val sequence = Mem(DisplayMessage(), initialContent = Array(
    CommandMessage(CMD_SWRESET, True),
    CommandMessage(CMD_SETEXTC),
    DataMessage(0xFF),
    DataMessage(0x83),
    DataMessage(0x57, True),
    CommandMessage(CMD_SETRGB),
    DataMessage(0x80),
    DataMessage(0x00),
    DataMessage(0x06),
    DataMessage(0x06),
    CommandMessage(CMD_SETVCOM),
    DataMessage(0x25),
    CommandMessage(CMD_SETOSC),
    DataMessage(0x68),
    CommandMessage(CMD_SETPANEL),
    DataMessage(0x05),
    CommandMessage(CMD_SETPOWER),
    DataMessage(0x00),
    DataMessage(0x15),
    DataMessage(0x1C),
    DataMessage(0x1C),
    DataMessage(0x83),
    DataMessage(0xAA),
    CommandMessage(CMD_SETSTBA),
    DataMessage(0x50),
    DataMessage(0x50),
    DataMessage(0x01),
    DataMessage(0x3C),
    DataMessage(0x1E),
    DataMessage(0x08),
    CommandMessage(CMD_SETCYC),
    DataMessage(0x02),
    DataMessage(0x40),
    DataMessage(0x00),
    DataMessage(0x2A),
    DataMessage(0x2A),
    DataMessage(0x0D),
    DataMessage(0x78),
    CommandMessage(CMD_SETGAMMA),
    DataMessage(0x02),
    DataMessage(0x0A),
    DataMessage(0x11),
    DataMessage(0x1d),
    DataMessage(0x23),
    DataMessage(0x35),
    DataMessage(0x41),
    DataMessage(0x4b),
    DataMessage(0x4b),
    DataMessage(0x42),
    DataMessage(0x3A),
    DataMessage(0x27),
    DataMessage(0x1B),
    DataMessage(0x08),
    DataMessage(0x09),
    DataMessage(0x03),
    DataMessage(0x02),
    DataMessage(0x0A),
    DataMessage(0x11),
    DataMessage(0x1d),
    DataMessage(0x23),
    DataMessage(0x35),
    DataMessage(0x41),
    DataMessage(0x4b),
    DataMessage(0x4b),
    DataMessage(0x42),
    DataMessage(0x3A),
    DataMessage(0x27),
    DataMessage(0x1B),
    DataMessage(0x08),
    DataMessage(0x09),
    DataMessage(0x03),
    DataMessage(0x00),
    DataMessage(0x01),
    CommandMessage(CMD_COLMOD),
    DataMessage(0x55),
    CommandMessage(CMD_MADCTL),
    DataMessage(0xC0),
    CommandMessage(CMD_TEON),
    DataMessage(0x00),
    CommandMessage(CMD_TEARLINE),
    DataMessage(0x00),
    DataMessage(0x02),
    CommandMessage(CMD_SLPOUT),
    DataMessage(0x00, True),
    CommandMessage(CMD_DISPON),
    DataMessage(0x00, True),
    CommandMessage(CMD_CASET),
    DataMessage(0x00),
    DataMessage(0x00),
    DataMessage(0x3F),
    DataMessage(0x01),
    CommandMessage(CMD_PASET),
    DataMessage(0x00),
    DataMessage(0x00),
    DataMessage(0xDF),
    DataMessage(0x01)
  ))

  val delay = new Area {
    val TIME = 300 ms
    val CYCLES = (TIME * ClockDomain.current.frequency.getValue).toBigInt

    val counter = Reg(UInt(log2Up(CYCLES) bits)) init(0)
    val active = RegInit(False)

    val delayComplete = RegInit(False)
    val nextCounter = UInt(counter.getWidth + 1 bits)
    nextCounter := U(False ## counter) - 1

    counter := nextCounter((counter.getWidth - 1) downto 0)
    delayComplete := nextCounter.msb

    when (delayComplete) {
      active := False
    }

    def reset() = {
      counter := CYCLES
      active := True
    }
  }

  val messageIndex = Reg(UInt(sequence.addressWidth bits)) init(0)
  val outputValid = RegInit(True)
  val message = Reg(DisplayMessage()) init(CommandMessage(CMD_SWRESET))

  io.output.valid := outputValid
  io.output.payload := message

  val nextMessageIndex = messageIndex + 1
  val nextMessage = sequence(nextMessageIndex)

  when (delay.active) {
    when (io.output.ready) {
      outputValid := False
    }
  } otherwise {
    when (io.output.ready | ~outputValid) {
      outputValid := True
      messageIndex := nextMessageIndex
      message := nextMessage
      when (nextMessage.hasDelay) {
        delay.reset()
      }
    }
  }

  io.last := nextMessageIndex === sequence.initialContent.length && ~delay.active
}

case class Display() extends Component {
  val io = new Bundle {
    val display = out(DisplayIo())
  }

  val WIDTH = 320
  val HEIGHT = 480
  val PIXELS = WIDTH * HEIGHT


  val chipSelect = Reg(Bool()) init(False)
  io.display.cs := chipSelect

  val mode = Reg(MessageMode()) init(MessageMode.DATA)
  io.display.cd := mode === MessageMode.DATA

  val write = Reg(Bool()) init(False)
  io.display.write := write & ClockDomain.current.readClockWire

  val data = Reg(UInt(8 bits)) init(0)
  io.display.data := data


  val stateMachine = new StateMachine {
    val byteIndex = Reg(UInt(log2Up(PIXELS * 3) bits))

    val initializer = DisplayInitializer()
    initializer.io.output.ready := False
    write := True

    val initState : State = new State() with EntryPoint {
      onEntry {
        chipSelect := False
      }
      whenIsActive {
        initializer.io.output.ready := True

        val message = initializer.io.output.payload
        mode := message.mode
        write := initializer.io.output.valid
        data := message.data

        when (initializer.io.last) {
          goto(drawSetupState)
        }
      }
    }

    val drawSetupState : State = new State {
      whenIsActive {
        mode := MessageMode.COMMAND
        data := 0x2C
        goto(drawState)
      }
    }

    val drawState : State = new State {
      onEntry {
        byteIndex := 0
      }
      whenIsActive {
        mode := MessageMode.DATA
        data := 0
        byteIndex := byteIndex + 1

        when (byteIndex === PIXELS * 3 - 1) {
          goto(frameWait)
        }
      }
    }

    val frameWait : State = new StateDelay(20 ms) {
      whenIsActive {
        write := False
      }
      whenCompleted {
        goto(drawSetupState)
      }
    }

  }
}
