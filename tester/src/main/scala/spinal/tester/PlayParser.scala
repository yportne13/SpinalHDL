package spinal.tester

import spinal.core._
import spinal.lib.hdl._
/*
object playParser extends App {

  class toplevel extends Component {
    val io = IOparser.run(
      """
         module ctrl(
      input cond0,
  input [15:0] addr,
  output output_valid,
  output signed [7:0] output_payload
)
      """
    )

    io.output_valid := False
    io.output_payload := -1
  }

  SpinalVerilog(new toplevel)
 
}
*/