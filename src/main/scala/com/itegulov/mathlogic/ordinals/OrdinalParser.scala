package com.itegulov.mathlogic.ordinals

import com.itegulov.mathlogic.ordinals
import org.parboiled2._

import scala.language.implicitConversions

/**
 * @author Daniyar Itegulov
 */
class OrdinalParser(val input: ParserInput) extends Parser {
  type RO = Rule1[Ordinal]
  implicit private def wrpStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
  }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
  = rule { a ~ zeroOrMore(wrpStr(divider) ~ a ~> b) }

  def equalityOrdinals: Rule1[(Ordinal, Ordinal)] =
    rule {
      oneOrMore(ordExpr).separatedBy("=") ~> ((sq: Seq[Ordinal]) => (sq.head, sq(1)))
    }

  def ordExpr: RO  = leftAssoc(summable, ordinals.+, "+")
  private def summable: RO = leftAssoc(mullable, ordinals.*, "*")
  private def mullable: RO = rule {
    oneOrMore(unary).separatedBy("^") ~> ((_: Seq[Ordinal]).reduceRight(ordinals.^))
  }
  private def unary: RO = rule { omega | numeric | ("(" ~ ordExpr ~ ")") }
  private def omega: RO = rule { ch('w') ~> (() => ω()) }
  private def numeric: RO =
    rule { capture(oneOrMore(anyOf("0123456789"))) ~>
      ((s: String) => Num(Integer.parseInt(s)))
    }
}
