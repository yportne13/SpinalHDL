package spinal.core.parameter

import spinal.core._
import spinal.core.internals.Multiplexer
import spinal.core.internals.BinaryMultiplexer
import _root_.spinal.core.internals.TypeBitsP
import spinal.core.internals.Operator
import spinal.core.internals.Expression
import _root_.spinal.core.internals.BinaryOperator
import spinal.core.internals.TypeBitsP

class SIntPAdd extends BinaryOperator {

  override def opName: String = "+"

  override def getTypeObject: Any = TypeBitsP(left.param.name)


  type T = SIntP
}

class SIntPSub extends BinaryOperator {

  override def opName: String = "-"

  override def getTypeObject: Any = TypeBitsP(left.param.name)


  type T = SIntP
}

class SIntPMul extends BinaryOperator {
  override def opName: String = "*"

  override def getTypeObject: Any = TypeBitsP(left.param.name)

  type T = SIntP
}

object SIntP {
  def apply(param: Parameter) = {new SIntP}.setParam(param)
}

class SIntP extends BaseType {

  var param: Parameter = null

  def setParam(p: Parameter) = {
    this.param = p
    this
  }

  def + (right: SIntP): SIntP = wrapBinaryOperator(right, new SIntPAdd)

  def -(right: SIntP): SIntP = wrapBinaryOperator(right, new SIntPSub)

  def *(right: SInt): SIntP = wrapBinaryOperator(right, new SIntPMul).setParam(this.param + right.getWidth)

  def *(right: SIntP): SIntP = wrapBinaryOperator(right, new SIntPMul).setParam(this.param + right.param)

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

  override private[core] def weakClone: this.type = SIntP(param).asInstanceOf[this.type]
  
}
