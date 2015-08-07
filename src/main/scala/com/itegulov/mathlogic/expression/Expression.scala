package com.itegulov.mathlogic.expression

/**
 * @author Daniyar Itegulov
 */
sealed trait Expression {
  def &&&(other: Expression): And = And(this, other)
  def |||(other: Expression): Or = Or(this, other)
  def -->(other: Expression): Impl = Impl(this, other)
}

case class Gap(name: String) extends Expression {
  override def toString: String = "Gap(" + name + ")"

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Gap => other.name == name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode
}

case class Not(exp: Expression) extends Expression {
  override def toString: String = "!(" + exp + ")"
}

case class And(left: Expression, right: Expression) extends Expression {
  override def toString: String = "(" + left + ")" + "&" + "(" + right + ")"
}

case class Or(left: Expression, right: Expression) extends Expression {
  override def toString: String = "(" + left + ")" + "|" + "(" + right + ")"
}

case class Impl(left: Expression, right: Expression) extends Expression {
  override def toString: String = "(" + left + ")" + "->" + "(" + right + ")"
}

case class ForAll(v: String, exp: Expression) extends Expression {
  override def toString: String = "@" + v + "(" + exp + ")"
}

case class Exists(v: String, exp: Expression) extends Expression {
  override def toString: String = "?" + v + "(" + exp + ")"
}

case class Equal(left: Function, right: Function) extends Expression {
  override def toString: String = "(" + left + ")" + "=" + "(" + right + ")"
}

case class Predicate(name: String, args: List[Function]) extends Expression {
  override def toString: String = name + args.mkString("(", ",", ")")
}