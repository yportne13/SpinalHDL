//this code is from https://github.com/fpinscala/fpinscala but i do some change on that. the license of fpinscala is:

//Copyright (c) 2012, Manning Publications, Co. 

//Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

//The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package spinal.lib.hdl

import fpinscala.parsing._
import language.higherKinds
import language.implicitConversions

trait Direction {
  def toString(): String
}

object Direction {
  case object In extends Direction {
    override def toString() = "in"
  }
  case object Out extends Direction {
    override def toString() = "out"
  }
  case object InOut extends Direction {
    override def toString() = "inout"
  }
}

trait DataType {
  def toMacro(): (String, Int)
}

object DataType {
  case object Bool extends DataType {
    override def toMacro() = ("Bool", -1)
  }
  case class Bits(width: Int) extends DataType {
    override def toMacro() = ("Bits", width)//
  }
  case class UInt(width: Int) extends DataType {//TODO:cannot get from verilog?
    override def toMacro() = ("UInt", width)//${width} bits
  }
  case class SInt(width: Int) extends DataType {
    override def toMacro() = ("SInt", width)//${width} bits
  }
  case class Vec(get: (DataType, Int)) extends DataType {//TODO:auto guess which is vector
    override def toMacro() = ("", -1)
    //override def toMacro() = s"Vec(${get._1.toString()},${get._2})"
  }
  case class Bundle(get: Map[String, DataType]) extends DataType {//TODO:auto guess which is bundle, and find out if is a spinal.lib build in bus
    override def toMacro() = ("", -1)
  }
}

trait Verilog

object Verilog {
  case class Signal(get: (Direction, String ,DataType)) extends Verilog
  case class Module(get: (String, List[Verilog])) extends Verilog

  def verilogParser[Parser[+_]](P: Parsers[Parser]): Parser[Verilog] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def strictName: Parser[String] = scope("signal or module name error") (
      regex("[a-zA-Z]+(([0-9]?[a-z]?[A-Z]?[_]?)*)".r)
    )

    def signal: Parser[Verilog] = {
      def direction: Parser[Direction] = scope("direction") (
        ("input".as(Direction.In) |
        "output".as(Direction.Out) |
        "inout".as(Direction.InOut)) <* ("reg" | "")
      )
      def size: Parser[Int] = scope("signal size") (
        "[" *> double.map(x => x.toInt+1) <* ":0]"//TODO:if not start from 0
      )
      def name: Parser[String] = (
        strictName
      )
      def signalBool: Parser[Signal] = (direction ** name).map{case (d,n) => Signal((d,n,DataType.Bool))}
      def signalBits: Parser[Signal] = (direction ** size ** name).map{case ((d,s),n) => Signal((d,n,DataType.Bits(s)))}
      def signalSInt: Parser[Signal] = ((direction <* "signed") ** size ** name).map{case ((d,s),n) => Signal((d,n,DataType.SInt(s)))}

      attempt(signalSInt) | attempt(signalBits) | signalBool
    }

    def module: Parser[Verilog] = {
      def modulename: Parser[String] = {
        "module" *> strictName <* ("(" | (whitespace *> "("))
      }

      def io: Parser[List[Verilog]] = {
        signal.sep(",")
      }

      (modulename ** io).map{case (n,s) => Module((n, s))}
    }

    whitespace *> module
  }
}

object VerilogParser {
  def apply(input: String): List[Verilog.Signal] = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val verilog: Parser[Verilog] = Verilog.verilogParser(P)
    val prepare = input.scanLeft(" "){case (l,c) =>
      if(l.last == '\\' && c == 'n') {
        l.dropRight(1)
      }else {
        l + c
      }
    }.last
    val parserOut = P.run(verilog)(prepare)
    parserOut match {
      case Left(a) => {println(a); List()}
      case Right(a) => {a match {
        case Verilog.Signal(_) => {List()}//no possible
        case Verilog.Module((name, signals: List[Verilog.Signal])) => signals
      }}
    }
  }
}

/**
 * Verilog parsing example.
 */
object VERILOGExample extends App {
  val jsonTxt =  """
    module ctrl(
      input cond0,
  input [15:0] addr,
  output b,
  output signed [7:0] output
);

assign output_valid = a

endmodule
    """
//"""
//module ctrl (
//  input io_a,
//  input [15:0] addr,
//  output io_b,
//  output signed [7:0] io_output
//)
//"""

  var malformedJson1 = """
\n    module ctrl(\n  input io_a,\n  input [15:0] addr,\n  output io_b,\n  output signed [7:0] io_output\n)\n 
"""

  malformedJson1 = malformedJson1.scanLeft(" "){case (l,c) =>
    if(l.last == '\\' && c == 'n') {
      l.dropRight(1)
    }else {
      l + c
    }
  }.last

  //println(malformedJson1)

  val malformedJson2 = """
 input [15:0] io_a
"""

  val P = fpinscala.parsing.Reference
  import fpinscala.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E,Verilog]) =
    e.fold(println, println)

  val verilog: Parser[Verilog] = Verilog.verilogParser(P)
  printResult { P.run(verilog)(jsonTxt) }
  println("--")
  printResult { P.run(verilog)(malformedJson1) }
  println("--")
  printResult { P.run(verilog)(malformedJson2) }
}
