package spinal.tester.sv

import spinal.core._
import spinal.lib._
import spinal.core.parameter._

class toplevel(width: Parameter = Parameter("width", 8)) extends Component {
  val io = new Bundle {
    //val x = in Vec(Bits(8 bits),16)
    val a = in(BitsP(width))
    val b = in(BitsP(width))
    val c = in(BitsP(width))
    val z = out(BitsP(width))
    val sa = in(SIntP(width))
    val sb = in(SIntP(width))
    val sc = out(SIntP(width+width))
  }

  io.z := io.a + io.b - io.c

  io.sc := io.sa * io.sb
  val sd = io.sa * io.sb
  sd.asOutput()

}

object testsv extends App {
  SpinalSystemVerilog(new toplevel)
}
