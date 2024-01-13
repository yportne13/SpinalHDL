package spinal.core.parameter

import spinal.core._
import spinal.core.internals.Multiplexer
import spinal.core.internals.BinaryMultiplexer
import _root_.spinal.core.internals.TypeBitsP
import spinal.core.internals.Operator
import spinal.core.internals.Expression
import _root_.spinal.core.internals.BinaryOperator

class BitsPAdd extends BinaryOperator {

  override def opName: String = "+"

  override def getTypeObject: Any = TypeBitsP(left.param.name)


  type T = BitsP
}

class BitsPSub extends BinaryOperator {

  override def opName: String = "-"

  override def getTypeObject: Any = TypeBitsP(left.param.name)


  type T = BitsP
}

case class BitsP(param: Parameter) extends BaseType {

  def + (right: BitsP): BitsP = wrapBinaryOperator(right, new BitsPAdd)

  def -(right: BitsP): BitsP = wrapBinaryOperator(right, new BitsPSub)

  def getWidth = 0

  override def getZero: this.type = this

  override def asBits: Bits = Bits(0 bits)

  override def assignFromBits(bits: Bits): Unit = {}

  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = {}

  override private[core] def isEqualTo(that: Any): Bool = False

  override private[core] def isNotEqualTo(that: Any): Bool = False

  override def getBitsWidth: Int = 0

  override def opName: String = "some_param"

  override def getTypeObject: Any = TypeBitsP(param.name)

  override private[core] def newMultiplexerExpression(): Multiplexer = null

  override private[core] def newBinaryMultiplexerExpression(): BinaryMultiplexer = null

  override private[core] def weakClone: this.type = new BitsP(param).asInstanceOf[this.type]
  
}
