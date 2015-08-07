package com.itegulov.mathlogic.expression

/**
 * @author Daniyar Itegulov
 */
sealed trait Function {
  def ***(other: Function): Mul = Mul(this, other)
  def +++(other: Function): Add = Add(this, other)
  def ===(other: Function): Equal = Equal(this, other)
}

case class Stroke(exp: Function) extends Function {
  override def toString: String = "(" + exp + ")" + "'"
}

case class Mul(left: Function, right: Function) extends Function {
  override def toString: String = "(" + left + ")" + "*" + "(" + right + ")"
}

case class Add(left: Function, right: Function) extends Function {
  override def toString: String = "(" + left + ")" +  "+" + "(" + right + ")"
}

case class Fun(name: String, args: List[Function]) extends Function {
  override def toString: String = name + args.mkString("(", ",", ")")
}

case class Var(name: String) extends Function {
  override def toString: String = name
}