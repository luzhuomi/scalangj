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
case object KW_Abstract extends JavaToken
case object KW_Assert extends JavaToken
case object KW_Boolean extends JavaToken
case object KW_Break extends JavaToken
case object KW_Byte extends JavaToken
case object KW_Case extends JavaToken
case object KW_Catch extends JavaToken
case object KW_Class extends JavaToken
case object KW_Const extends JavaToken
case object KW_Continue extends JavaToken
case object KW_Default extends JavaToken
case object KW_Do extends JavaToken
case object KW_Double extends JavaToken
case object KW_Else extends JavaToken
case object KW_Enum extends JavaToken
case object KW_Extends extends JavaToken
case object KW_Final extends JavaToken
case object KW_Finally extends JavaToken
case object KW_Float extends JavaToken
case object KW_For extends JavaToken
case object KW_Goto extends JavaToken
case object KW_If extends JavaToken
case object KW_Implements extends JavaToken
case object KW_Import extends JavaToken
case object KW_Instanceof extends JavaToken
case object KW_Int extends JavaToken
case object KW_Interface extends JavaToken
case object KW_Long extends JavaToken
case object KW_Native extends JavaToken
case object KW_New extends JavaToken
case object KW_Package extends JavaToken
case object KW_Private extends JavaToken
case object KW_Protected extends JavaToken
case object KW_Public extends JavaToken
case object KW_Return extends JavaToken
case object KW_Short extends JavaToken
case object KW_Static extends JavaToken
case object KW_Strictfp extends JavaToken
case object KW_Super extends JavaToken
case object KW_Switch extends JavaToken
case object KW_Synchronized extends JavaToken
case object KW_This extends JavaToken
case object KW_Throw extends JavaToken
case object KW_Transient extends JavaToken
case object KW_Try extends JavaToken
case object KW_Void extends JavaToken
case object KW_Volatile extends JavaToken
case object KW_While extends JavaToken
case class IndentTok(v:String) extends JavaToken
case object OpenParam extends JavaToken
case object CloseParam extends JavaToken
case object OpenSquare extends JavaToken
case object CloseSqaure extends JavaToken
case object OpenCurly extends JavaToken
case object CloseCurly extends JavaToken
case object SemiColon extends JavaToken
case object Comma extends JavaToken
case object Period extends JavaToken
case object LambdaArrow extends JavaToken
case object MethodRefSep extends JavaToken
case object Op_Equal extends JavaToken
case object Op_GThan extends JavaToken
case object Op_LThan extends JavaToken

