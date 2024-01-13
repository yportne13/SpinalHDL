package spinal.core.parameter

case class Parameter(name: String, default: Int = -1) {
  def +(that: Int) = Parameter(s"${name}+${that}")
  def +(that: Parameter) = Parameter(s"${name}+${that.name}")
  def -(that: Int) = Parameter(s"${name}-${that}")
  def -(that: Parameter) = Parameter(s"${name}-${that.name}")
  def *(that: Int) = Parameter(s"${name}*${that}")
  def *(that: Parameter) = Parameter(s"${name}*${that.name}")
}
