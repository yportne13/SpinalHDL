package spinal.lib.hdl

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import spinal.core._

object IOparser {

  def runImpl(c: whitebox.Context)(raw: c.Tree): c.Tree = {
    import c.universe._
    val signal = VerilogParser(raw.toString().drop(1).dropRight(1))
                  .map(x => (x.get._1.toString(), x.get._2, x.get._3.toMacro()))
                  .map(x => if(x._3._2 >= 0) q"""val ${TermName(x._2)} = ${TermName(x._1)} (new ${TypeName(x._3._1)}).setWidth(${x._3._2})"""
                            else q"""val ${TermName(x._2)} = ${TermName(x._1)} ${TermName(x._3._1)}()""").toList
    val ret = q"""new Bundle {..$signal}"""
    ret
  }

  def run(raw: String): Bundle = macro runImpl

}

