package com.github.luzhuomi.scalangj
// https://argodis.de/blog/2019-05-09/formula-parsing-with-scala-lexer/index.html
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator._
import org.apache.commons.lang3.StringEscapeUtils._

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val digit:Regex = "[0-9]".r
  val nonzero:Regex = "[1-9]".r
  val octdig:Regex = "[0-7]".r
  val hexdig:Regex = "[0-9A-Fa-f]".r

  val lineterm:Regex = "[\n\r] | \r\n".r 
  val tradcom_in = s"( ~[\\*] | \\*+ (~[\\/\\*] | \n) | \n )* \\*+".r
  val tradcomm:Regex = s"/*${tradcom_in}/".r
  val linecomm:Regex = s"//.*${lineterm}".r
  val comm:Regex = s"${tradcomm}|${linecomm}".r 

  val octEscape:Regex = s"[0123]?${octdig}{1,2}".r
  val hexEscape:Regex = s"u${hexdig}{4}".r
  val charEscape:Regex = s"""(${octEscape}|${hexEscape}|[btnfr\"\'\\\\])""".r
  // val charEscape:Regex = s"""\\\\(${octEscape}|${hexEscape})""".r
  val expsuffix:Regex = s"[+-]?${digit}+".r
  val exponent:Regex = s"[eE]${expsuffix}".r 
  val pexponent:Regex = s"[pP]${expsuffix}".r


  def p_ann_interface: Parser[JavaToken] = "@interface" ^^ { _ => KW_AnnInterface }

  def p_abstract: Parser[JavaToken] = "abstract" ^^ { _ => KW_Abstract }

  def p_assert: Parser[JavaToken] = "assert" ^^ { _ => KW_Assert }

  def p_boolean: Parser[JavaToken] = "boolean" ^^ { _ => KW_Boolean }

  def p_break: Parser[JavaToken] = "break" ^^ { _ => KW_Break }

  def p_byte: Parser[JavaToken] = "byte" ^^ {_ => KW_Byte }

  def p_case: Parser[JavaToken] = "case" ^^ {_ => KW_Case } 

  def p_catch: Parser[JavaToken] = "catch" ^^ {_ => KW_Catch } 

  def p_char: Parser[JavaToken] = "char" ^^ {_ => KW_Char } 

  def p_class: Parser[JavaToken] = "class" ^^ {_ => KW_Class }

  def p_const: Parser[JavaToken] = "const" ^^ {_ => KW_Const }

  def p_continue: Parser[JavaToken] = "continue" ^^ {_ => KW_Continue }

  def p_default: Parser[JavaToken] = "default" ^^ {_ => KW_Default }

  def p_do: Parser[JavaToken] = "do" ^^ {_ => KW_Default }

  def p_double: Parser[JavaToken] = "double" ^^ { _ => KW_Double }

  def p_else: Parser[JavaToken] = "else" ^^ { _ => KW_Else }

  def p_enum: Parser[JavaToken] = "enum" ^^ { _ => KW_Enum }

  def p_extends: Parser[JavaToken] = "extends" ^^ { _ => KW_Extends } 

  def p_final: Parser[JavaToken] = "final" ^^ { _ => KW_Final }

  def p_finally: Parser[JavaToken] = "finally" ^^ { _ => KW_Finally }

  def p_float: Parser[JavaToken] = "float" ^^ { _ => KW_Float }

  def p_for: Parser[JavaToken] = "for" ^^ {_ => KW_For }

  def p_goto: Parser[JavaToken] = "goto" ^^ { _ => KW_Goto }
  
  def p_if: Parser[JavaToken] = "if" ^^ { _ => KW_If }

  def p_implements: Parser[JavaToken] = "implements" ^^ { _ => KW_Implements }

  def p_import: Parser[JavaToken] = "import" ^^ { _ => KW_Import } 

  def p_instanceof: Parser[JavaToken] = "instanceof" ^^ { _ => KW_Instanceof }

  def p_int: Parser[JavaToken] = "int" ^^ { _ => KW_Int }

  def p_interface: Parser[JavaToken] = "interface" ^^ { _ => KW_Interface }

  def p_long: Parser[JavaToken] = "long" ^^ { _ => KW_Long }

  def p_native: Parser[JavaToken] = "native" ^^ { _ => KW_Native } 

  def p_new: Parser[JavaToken] = "new" ^^ {_ => KW_New } 

  def p_package: Parser[JavaToken] = "package" ^^ {_ => KW_Package} 

  def p_private: Parser[JavaToken] = "private" ^^ { _ => KW_Private } 

  def p_protected: Parser[JavaToken] = "proected" ^^ { _ => KW_Protected } 

  def p_public: Parser[JavaToken] = "public" ^^ {_ => KW_Public }

  def p_return: Parser[JavaToken] = "return" ^^ { _ => KW_Return }

  def p_short: Parser[JavaToken] = "short" ^^ { _ => KW_Short } 

  def p_static: Parser[JavaToken] = "static" ^^ { _ => KW_Static }

  def p_strictfp: Parser[JavaToken] = "strictfp" ^^ { _ => KW_Strictfp }

  def p_super: Parser[JavaToken] = "super" ^^ { _ => KW_Super } 

  def p_switch: Parser[JavaToken] = "switch" ^^ { _ => KW_Switch } 

  def p_syncronized : Parser[JavaToken] = "synchronized" ^^ { _ => KW_Synchronized }

  def p_this : Parser[JavaToken] = "this" ^^ { _ => KW_This } 

  def p_throw : Parser[JavaToken] = "throw" ^^ { _ => KW_Throw }

  def p_throws : Parser[JavaToken] = "throws" ^^ { _ => KW_Throws }

  def p_transient : Parser[JavaToken] = "transient" ^^ {_ => KW_Transient }

  def p_try : Parser[JavaToken] = "try" ^^ {_ => KW_Try }

  def p_void : Parser[JavaToken] = "void" ^^ {_ => KW_Void }

  def p_volatile : Parser[JavaToken] = "volatile" ^^ {_ => KW_Volatile }

  def p_while : Parser[JavaToken] = "while" ^^ { _ => KW_While } 

  def p_IntTok: Parser[JavaToken] = {
    s"${nonzero}${digit}*".r ^^ { s => IntTok(s.toInt) } |
    s"0[xX]${hexdig}+".r ^^ {s => IntTok(java.lang.Integer.decode(s))} |
    s"0${digit}+".r ^^ { s => IntTok(s.toInt) } |
    "0" ^^ { _ => IntTok(0) }
  }

  def p_LongTok: Parser[JavaToken] = {
    s"${nonzero}${digit}*[lL]".r ^^ { s => LongTok(s.toLong) } |
    s"0[xX]${hexdig}+[lL]".r ^^ {s => LongTok(java.lang.Long.decode(s))} |
    s"0${digit}+[lL]".r ^^ { s => LongTok(s.toLong) } 
    "0[lL]".r ^^ { _ => LongTok(0) } 
  }

  def p_DoubleTok: Parser[JavaToken] = {
    s"${digit}+\.${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } | 
    s"\.${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } | 
    s"${digit}+${exponent}".r ^^ { s => DoubleTok(s.toDouble) } |
    s"${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } |
    s"0[xX]${hexdig}*\.?${hexdig}*${pexponent}[dD]?".r ^^ { s => DoubleTok{s.toDouble}} 
  }

  def p_FloatTok: Parser[JavaToken] = {
    s"${digit}+\.${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } | 
    s"\.${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } | 
    s"${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } |
    s"0[xX]${hexdig}*\.?${hexdig}*${pexponent}[fF]?".r ^^ { s => FloatTok{s.toFloat}} 
  }


  def p_BoolTok: Parser[JavaToken] = {
    "true" ^^ { s => BoolTok(true)} | 
    "false" ^^ { s => BoolTok(false)}
  }

  def readChartok(s:String):Char = {
    convChar(dropQuotes(s)).head
  }

  def dropQuotes(s:String):String = {
    s.tail.take(s.length() - 2)
  }

  val atof = (('a' to 'f') ++ ('A' to 'F')).toSet

  val octs = ('0' to '7').toSet

  def isHexDigit(c:Char) : Boolean = ((c.isDigit) || (atof(c)))

  def isOctDigit(c:Char) : Boolean = octs(c)

  def toEnum(s:String):Char = unescapeJava(s).charAt(0)

  def convChar(s:String):String = convChar1(s.toList).mkString 

  def convChar1(cs:List[Char]):List[Char] = cs match {
    case '\\'::'u'::d1::d2::d3::d4::rest if (List(d1,d2,d3,d4).forall(isHexDigit(_))) => 
    {
      val c = toEnum(List('\\','u',d1,d2,d3,d4).mkString)
      val cs2 = convChar1(rest)
      c::cs2
    } 
    case '\\'::c::rest => 
    {
      if (isOctDigit(c)) {
        val maxRemainingOctals = if (c <= '3') { 2 } else {1}
        def convOctal(n:Int) :Char = {
          val octals = rest.take(n).takeWhile(isOctDigit)
          val noctals = octals.length
          val toChar = (s) => toEnum(readOct(s).head._1)
          toChar((c::octals))::convChar1(rest.drop(noctals))
        }
        convOctal(maxRemainingOctals)
      } else {
        List()
      }
    }
  }

  def p_CharTok:Parser[JavaToken] = {
    // s"'${charEscape}|~[\\\']'".r ^^ { s => CharTok(s.toCharArray()(0)) } 
    s"'[^\\\']'".r ^^ { s => CharTok(s.toCharArray()(1)) }
  }

  def p_StringTok:Parser[JavaToken] = {
    s"""\"${charEscape}|[^\\\']'""".r ^^ { s => StringTok(s) } 
  }

  def p_Null:Parser[JavaToken] = "null" ^^ { _ => NullTok }


  def parse_one(p:Parser[JavaToken], src: String): ParseResult[JavaToken] =
    parse(phrase(p), src)





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
