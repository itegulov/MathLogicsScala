package com.itegulov.mathlogic

/**
 * @author Daniyar Itegulov
 */
package object ordinals {
  val zero = Atom(BigInt(0))
  val one = Atom(BigInt(1))

  implicit def bigintpow(b: BigInt): Object {def powB(that: BigInt): BigInt} = new {
    def powB(that: BigInt): BigInt =
      if (that.isValidInt)
        b pow that.intValue()
      else
        throw new Exception("exp is too big")
  }
}
