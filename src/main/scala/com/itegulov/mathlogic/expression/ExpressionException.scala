package com.itegulov.mathlogic.expression

/**
 * @author Daniyar Itegulov
 */
class ExpressionException(exp: Expression) extends Exception

class UnsafeForSubstException(exp: Expression) extends ExpressionException(exp)

class FreeOccurrenceException(exp: Expression) extends ExpressionException(exp)

class BadRuleException(exp: Expression) extends ExpressionException(exp)
