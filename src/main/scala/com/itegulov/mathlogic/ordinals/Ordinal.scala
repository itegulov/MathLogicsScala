package com.itegulov.mathlogic.ordinals

/**
 * @author Daniyar Itegulov
 */
sealed trait Ordinal {
  def convertToCNF: CNF
}

case class *(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = "(" + left + "*" + right + ")"

  override def convertToCNF: CNF = ???
}

case class +(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = "(" + left + "+" + right + ")"

  override def convertToCNF: CNF = ???
}

case class ^(left: Ordinal, right: Ordinal) extends Ordinal {
  override def toString: String = "(" + left + "^" + right + ")"

  override def convertToCNF: CNF = ???
}

case class Num(number: Int) extends Ordinal {
  override def toString: String = number.toString

  override def convertToCNF: CNF = ???
}

case class ω() extends Ordinal {
  override def toString: String = "ω"

  override def convertToCNF: CNF = ???
}