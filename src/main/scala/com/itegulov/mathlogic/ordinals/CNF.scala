package com.itegulov.mathlogic.ordinals

import scala.language.reflectiveCalls

object CNF {
  def naturalToInf(p: Atom, b: CNF): CNF = b match {
    case a if b.biggestPow == one =>
      Poly(List(ωElement(Atom(b.coefficientOfBiggestPow), p.num powB b.tail.asInstanceOf[Atom].num)),
        zero)
    case a if b.tail.isInstanceOf[Atom] =>
      Poly(List(ωElement(Poly(List(ωElement(b.biggestPow - one, b.coefficientOfBiggestPow)), zero),
                         p.num powB b.tail.asInstanceOf[Atom].num)),
        zero)
    case _ =>
      val c = naturalToInf(p, b.tail)
      Poly(List(ωElement(ωElement(b.biggestPow - one, b.coefficientOfBiggestPow) + c.biggestPow, c.coefficientOfBiggestPow)), zero)
  }

  def limitToNaturalPow(a: CNF, q: Atom): CNF = q match {
    case `one` => a
    case Atom(n) => Poly(List(ωElement(a.biggestPow * Atom(n - 1), 1)), zero) * a
  }

  def infiniteToNaturalPow(a: CNF, q: Atom): CNF = q match {
    case `zero` => one
    case `one` => a
    case x if a.isFinite => limitToNaturalPow(a, q)
    case Atom(n) => infiniteToNaturalPow(a, Atom(n - 1)) * a
  }

  def infiniteToInfinite(a: CNF, b: CNF): CNF =
    Poly(List(ωElement(a.biggestPow * b.greatestFiniteSubOrdinal, 1)), zero) * infiniteToNaturalPow(a, b.naturalPart)
}

/**
 * @author Daniyar Itegulov
 */
sealed trait CNF extends Ordered[CNF] {
  override def compare(other: CNF): Int = this match {
    case Atom(a) => other match {
      case Atom(b) => a compare b
      case _ => -1
    }
    case Poly(_, _) if other.isInstanceOf[Atom] => 1
    case Poly(_, _) if biggestPow != other.biggestPow =>
      biggestPow compare other.biggestPow
    case Poly(_, _) if coefficientOfBiggestPow != other.coefficientOfBiggestPow =>
      coefficientOfBiggestPow compare other.coefficientOfBiggestPow
    case _ => tail compare other.tail
  }

  def +(that: CNF): CNF = this match {
    case Atom(n) if (that match {
      case Atom(m) => true
      case _ => false
    }) => Atom(n + that.asInstanceOf[Atom].num)
    case _ =>
      if (biggestPow < that.biggestPow)
        that
      else
      if (biggestPow == that.biggestPow)
        ωElement(biggestPow, coefficientOfBiggestPow + that.coefficientOfBiggestPow) + that.tail
      else
        ωElement(biggestPow, coefficientOfBiggestPow) + (tail + that)
  }

  def -(that: CNF): CNF = this match {
    case Atom(n) if that.isInstanceOf[Atom] => that match {
      case Atom(m) => Atom(if (n <= m) 0 else n - m)
      case _ => throw new Exception("scala has broken, piu ((99(");
    }
    case a if biggestPow < that.biggestPow => zero
    case a if biggestPow > that.biggestPow => this
    case a if coefficientOfBiggestPow < that.coefficientOfBiggestPow => zero
    case a if coefficientOfBiggestPow > that.coefficientOfBiggestPow =>
      ωElement(biggestPow, coefficientOfBiggestPow - that.coefficientOfBiggestPow) + a.tail
    case _ => tail - that.tail
  }

  def *(that: CNF): CNF = that match {
    case `zero` => zero
    case a if this == zero => zero
    case Atom(m) => this match {
      case Atom(n) => Atom(n * m)
      case _ => ωElement(this.biggestPow, this.coefficientOfBiggestPow * m) + tail
    }
    case _ => ωElement(this.biggestPow + that.biggestPow, that.coefficientOfBiggestPow) + (this * that.tail)
  }


  def ^(that: CNF) = this match {
    case `one` => one
    case a if that == zero => one
    case `zero` => zero
    case t@Atom(n) => that match {
      case Atom(m) => Atom(n powB m)
      case _ => CNF.naturalToInf(t, that)
    }
    case a if that.isInstanceOf[Atom] => CNF.infiniteToNaturalPow(this, that.asInstanceOf[Atom])
    case _ => CNF.infiniteToInfinite(this, that)
  }

  def biggestPow: CNF

  def coefficientOfBiggestPow: BigInt

  def tail: CNF

  def length: Int

  def size: Int

  def firstElement: CNF

  def isFinite: Boolean

  def greatestFiniteSubOrdinal: CNF

  def naturalPart: Atom
}

case class Atom(num: BigInt) extends CNF {
  assert(num >= 0, "Atom cannot be negative")

  override def toString: String = num.toString()

  override def biggestPow: CNF = zero

  override def coefficientOfBiggestPow: BigInt = num

  override def tail: CNF = throw new IllegalStateException("Can't get tail from Atom")

  override def length: Int = 0

  override def size: Int = 1

  override def isFinite: Boolean = num == BigInt(0)

  override def greatestFiniteSubOrdinal: CNF = zero

  override def firstElement: CNF = this

  override def naturalPart: Atom = this
}

case class ωElement(pow: CNF, coefficient: BigInt) {
  override def toString: String = "(ω" +
    (if (pow != one) "^(" + pow + ")" else "") + ")" +
    (if (coefficient != BigInt(1)) "*" + coefficient else "")

  def +(other: CNF): Poly = other match {
    case atom: Atom => Poly(List(this), atom)
    case Poly(elements, atom) => Poly(this :: elements, atom)
  }
}

case class Poly(elements: List[ωElement], atom: Atom) extends CNF {
  assert(elements.nonEmpty, "Poly can't be empty")
  assert(elements.map(a => a.pow).distinct.length == elements.length, "All elements in Poly must be distinct")
  for (i <- 1 to elements.length - 1)
    assert(elements(i - 1).pow > elements(i).pow, "All elements in Poly must be ordered")

  override def toString: String = elements.map(a => a.toString).mkString("+") + (if (atom != zero) "+" + atom else "")

  override def biggestPow: CNF = elements.head.pow

  override def coefficientOfBiggestPow: BigInt = elements.head.coefficient

  override def tail: CNF = if (elements.length == 1) atom else Poly(elements.tail, atom)

  override def length: Int = 1 + tail.length

  override def size: Int = biggestPow.size + tail.size

  override def isFinite: Boolean = tail.isFinite

  override def greatestFiniteSubOrdinal: CNF = firstElement match {
    case Poly(l1, _) => tail.greatestFiniteSubOrdinal match {
      case t@Atom(n) => Poly(l1, t)
      case Poly(l2, a) => Poly(l1 ::: l2, a)
    }
    case _ => throw new IllegalStateException("Something got wrong: can't get finite subOrdinal from such Poly")
  }

  override def firstElement: CNF = Poly(elements.head :: Nil, Atom(0))

  override def naturalPart: Atom = tail.naturalPart
}