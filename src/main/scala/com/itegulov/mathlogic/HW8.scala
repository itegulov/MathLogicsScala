package com.itegulov.mathlogic

import com.itegulov.mathlogic.ordinals.OrdinalParser

import scala.io.Source

/**
 * @author Daniyar Itegulov
 */
object HW8 extends App {
  new OrdinalParser(Source.fromFile("task8.in", "UTF-8").mkString).equalityOrdinals.run().get match {
    case (a, b) => println(if (a.convertToCNF == b.convertToCNF) "Равны" else "Не равны")
  }
}
