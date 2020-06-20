package com.github.luzhuomi.scalangj
// https://argodis.de/blog/2019-05-09/formula-parsing-with-scala-lexer/index.html
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val digit:Regex = "[0-9]".r
  val nonzero:Regex = "[1-9]".r
  val octdig:Regex = "[0-7]".r
  val hexdig:Regex = "[0-9A-Fa-f]".r

  val lineterm:Regex = "[\n\r] | \r\n".r 
  val tradcom_in = s"( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+".r
  val tradcomm:Regex = s"/*${tradcom_in}/".r
  val linecomm:Regex = s"//.*${lineterm}".r
  val comm:Regex = s"${tradcomm}|${linecomm}".r 


  def ann_interface: Parser[JavaToken] = "@interface" ^^ { _ => KW_AnnInterface}


  def parse_one(p:Parser[JavaToken], src: String): ParseResult[JavaToken] =
    parse(p, src)


  /*
  // Braces
  def brace_left: Parser[FormulaToken] = "(" ^^ { _ => BRACE_LEFT }
  def brace_right: Parser[FormulaToken] = ")" ^^ { _ => BRACE_RIGHT }
  // Algebraic operators
  def operator_multiply: Parser[FormulaToken] = "*" ^^ { _ =>
    OPERATOR_MULTIPLY
  }
  def operator_divide: Parser[FormulaToken] = "/" ^^ { _ => OPERATOR_DIVIDE }
  def operator_add: Parser[FormulaToken] = "+" ^^ { _ => OPERATOR_ADD }
  def operator_subtract: Parser[FormulaToken] = "-" ^^ { _ =>
    OPERATOR_SUBTRACT
  }
  // Variable
  def variable: Parser[FormulaToken] = "$" ~> """\d+""".r ^^ { id =>
    VARIABLE(id.toInt)
  }
  // Constant
  def constant: Parser[FormulaToken] = {
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ { value =>
      CONSTANT(value.toDouble)
    }
  }

  def tokens: Parser[List[FormulaToken]] =
    phrase(
      rep1(
        brace_left
          | variable
          | constant
          | brace_right
          | operator_add
          | operator_subtract
          | operator_multiply
          | operator_divide
      )
    ) ^^ { rawTokens => rawTokens }

  def tokenize(formula: String): ParseResult[List[FormulaToken]] =
    parse(tokens, formula)
    */
  
}
