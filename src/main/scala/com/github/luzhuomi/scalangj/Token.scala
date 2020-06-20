package com.github.luzhuomi.scalangj

/*
sealed trait FormulaToken
case object BRACE_LEFT extends FormulaToken
case object BRACE_RIGHT extends FormulaToken
case class VARIABLE(id: Int) extends FormulaToken
case class CONSTANT(value: Double) extends FormulaToken
case object OPERATOR_ADD extends FormulaToken
case object OPERATOR_SUBTRACT extends FormulaToken
case object OPERATOR_MULTIPLY extends FormulaToken
case object OPERATOR_DIVIDE extends FormulaToken
*/

sealed trait JavaToken
case class DIGIT(value:String) extends JavaToken
case class NONZERO(value:String) extends JavaToken
case class OCTDIG(value:String) extends JavaToken
case class HEXDIG(value:String) extends JavaToken


// case class TRADCOMM(value:String) extends JavaToken
// case class LINECOMM(value:String) extends JavaToken
case class COMM(value:String) extends JavaToken

case object KW_AnnInterface extends JavaToken