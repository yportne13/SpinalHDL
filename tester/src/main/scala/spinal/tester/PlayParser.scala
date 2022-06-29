package spinal.tester

import spinal.core._
import spinal.lib.hdl._

object playParser extends App {

  class bb extends BlackBox {
    val io = IOparser.run(
    """
        module ctrl(
      input cond0,
  input [15:0] addr,
  output b,
  output signed [7:0] output
);

assign output_valid = a

endmodule
    """
    )

    noIoPrefix()

  }

  class toplevel extends Component {
    val io = new Bundle {
      val a = in Bool()
      val addr = in Bits(16 bits)
      val b = out Bool()
      val output = out (Reg(SInt(8 bits)))
    }

    val black = new bb
    black.io.addr := io.addr
    black.io.cond0 := io.a

    io.b := black.io.output_valid
    io.output := black.io.output_payload

  }

  SpinalVerilog(new toplevel)
 
}
