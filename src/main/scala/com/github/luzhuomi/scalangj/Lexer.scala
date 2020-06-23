package com.github.luzhuomi.scalangj
// https://argodis.de/blog/2019-05-09/formula-parsing-with-scala-lexer/index.html
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator._
import org.apache.commons.lang3.StringEscapeUtils._
import scala.collection.convert.DecorateAsJava

object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\n\f]+".r
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

  val javaLetter:Regex = """[a-zA-Z\_\$]""".r
  val javaDigit:Regex = digit
  val javaLetterOrDigit:Regex = """[a-zA-Z0-9\_\$]""".r

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

  def p_synchronized : Parser[JavaToken] = "synchronized" ^^ { _ => KW_Synchronized }

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
    s"${nonzero}${digit}*[lL]".r ^^ { s => LongTok(java.lang.Long.decode(s.take(s.length()-1))) }  |
    s"0[xX]${hexdig}+[lL]".r ^^ {s => LongTok(java.lang.Long.decode(s.take(s.length()-1)))}  |
    s"0${digit}+[lL]".r ^^ {s => LongTok(java.lang.Long.decode(s.take(s.length()-1)))} |  
    "0[lL]".r ^^ { _ => LongTok(0) } 
  }

  def p_DoubleTok: Parser[JavaToken] = {
    s"${digit}+[.]${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } | 
    s"[.]${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } | 
    s"${digit}+${exponent}".r ^^ { s => DoubleTok(s.toDouble) } |
    s"${digit}+${exponent}?[dD]?".r ^^ { s => DoubleTok(s.toDouble) } |
    s"0[xX]${hexdig}*[.]?${hexdig}*${pexponent}[dD]?".r ^^ { s => DoubleTok{s.toDouble}} 
  }

  def p_FloatTok: Parser[JavaToken] = {
    s"${digit}+[.]${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } | 
    s"[.]${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } | 
    s"${digit}+${exponent}?[fF]?".r ^^ { s => FloatTok(s.toFloat) } |
    s"0[xX]${hexdig}*[.]?${hexdig}*${pexponent}[fF]?".r ^^ { s => FloatTok{s.toFloat}} 
  }


  def p_BoolTok: Parser[JavaToken] = {
    "true" ^^ { s => BoolTok(true)} | 
    "false" ^^ { s => BoolTok(false)}
  }

  def readCharTok(s:String):Char = {
    convChar(dropQuotes(s)).head
  }

  def readStringTok(s:String):String = {
    convChar(dropQuotes(s))   
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


  def readOct(cs:List[Char]):Int = cs match { // only consider 3 digits max
    case d1::d2::d3::_ => (d1 - '0') * 8 * 8 + (d2 - '0') * 8 + (d3 - '0') 
    case d1::d2::Nil => (d1 - '0') * 8 + (d2 - '0')
    case d1::Nil => d1 - '0'
    case Nil => lexicalError("readOct is applied to an empty list of character, this should not happen.")
  } 

  def lexicalError(mesg:String) = {
    sys.error(s"lexical error: ${mesg}")
  }

  def convChar1(cs:List[Char]):List[Char] = cs match {
    case '\\'::'u'::d1::d2::d3::d4::rest if (List(d1,d2,d3,d4).forall(isHexDigit(_))) => 
    {
      val c = toEnum(List('\\','u',d1,d2,d3,d4).mkString)
      val cs2 = convChar1(rest)
      c::cs2
    }
    case '\\'::'u'::rest => lexicalError(s"""bad unicode escape \"\\u${rest.take(4)}\"""") 
    case '\\'::c::rest => 
    {
      if (isOctDigit(c)) {
        val maxRemainingOctals = if (c <= '3') { 2 } else {1}
        def convOctal(n:Int):List[Char] = {
          val octals = rest.take(n).takeWhile(isOctDigit)
          val noctals = octals.length
          def toChar(cs:List[Char]):Char = readOct(cs).toChar
          (toChar((c::octals)))::(convChar1(rest.drop(noctals)))
        }
        convOctal(maxRemainingOctals)
      } else { 
        val hd = c match { 
          case 'b' => '\b'
          case 'f' => '\f'
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case '\'' => '\''
          case '\\' => '\\'
          case '"' => '"'
          case _ => lexicalError(s"""bad escape \"\\" ${c}"\" """)
        }
        hd::convChar1(rest)
      }
    }
    case '\\'::List() => lexicalError(s"""bad escape \"\\\" """)
    case (x::s) => x::convChar1(s)
    case List() => List()
  }

  def p_CharTok:Parser[JavaToken] = {
    s"'(${charEscape}|[^\\\'])'".r ^^ { s => CharTok(readCharTok(s)) } 
    // s"'[^\\\']'".r ^^ { s => CharTok( readCharTok(s)) }
  }

  def p_StringTok:Parser[JavaToken] = {
    s"""\"(${charEscape}|[^\\\'])*\"""".r ^^ { s => StringTok(readStringTok(s)) } 
  }

  def p_NullTok:Parser[JavaToken] = "null" ^^ { _ => NullTok }

  def p_IdentTok:Parser[JavaToken] = s"${javaLetter}${javaLetterOrDigit}*".r ^^ {
    s => IdentTok(s)
  }

  def p_OpenParen:Parser[JavaToken] = "(" ^^ { _ => OpenParen }
  def p_CloseParen:Parser[JavaToken] = ")" ^^ { _ => CloseParen }
  def p_OpenSquare:Parser[JavaToken] = "[" ^^ { _ => OpenSquare }
  def p_CloseSquare:Parser[JavaToken] = "]" ^^ { _ => CloseSqaure }
  def p_OpenCurly:Parser[JavaToken] = "{" ^^ { _ => OpenCurly }
  def p_CloseCurly:Parser[JavaToken] = "}" ^^ { _ => CloseCurly }
  def p_SemiColon:Parser[JavaToken] = ";" ^^ { _ => SemiColon } 
  def p_Comma:Parser[JavaToken] = "," ^^ { _ => Comma }
  def p_Period:Parser[JavaToken] = "." ^^ { _ => Period }
  def p_LambdaArrow:Parser[JavaToken] = "->" ^^ { _ => LambdaArrow }
  def p_MethodRefSep:Parser[JavaToken] = "::" ^^ { _ => MethodRefSep }

  def p_Op_Equal:Parser[JavaToken] = "=" ^^ { _ => Op_Equal }
  def p_Op_GThan:Parser[JavaToken] = ">" ^^ { _ => Op_GThan }
  def p_Op_LThan:Parser[JavaToken] = "<" ^^ { _ => Op_LThan }
  def p_Op_Bang:Parser[JavaToken] = "!" ^^ { _ => Op_Bang }
  def p_Op_Tilde:Parser[JavaToken] = "~" ^^ { _ => Op_Tilde }
  def p_Op_Query:Parser[JavaToken] = "?" ^^ { _ => Op_Query }
  def p_Op_Colon:Parser[JavaToken] = ":" ^^ { _ => Op_Colon }
  def p_Op_Equals:Parser[JavaToken] = "==" ^^ { _ => Op_Equals }
  def p_Op_LThanE:Parser[JavaToken] = "<=" ^^ { _ => Op_LThanE }
  def p_Op_GThanE:Parser[JavaToken] = ">=" ^^ { _ => Op_GThanE }
  def p_Op_BangE:Parser[JavaToken] = "!=" ^^ { _ => Op_BangE }
  def p_Op_AAnd:Parser[JavaToken] = "&&" ^^ { _ => Op_AAnd } 
  def p_Op_OOr:Parser[JavaToken] = "||" ^^ { _ => Op_OOr }
  def p_Op_PPlus:Parser[JavaToken] = "++" ^^ { _ => Op_PPlus }
  def p_Op_MMinus:Parser[JavaToken] = "--" ^^ { _ => Op_MMinus }
  def p_Op_Plus:Parser[JavaToken] = "+" ^^ { _ => Op_Plus } 
  def p_Op_Minus:Parser[JavaToken] = "-" ^^ { _ => Op_Minus }
  def p_Op_Star:Parser[JavaToken] = "*" ^^ { _ => Op_Star }
  def p_Op_Slash:Parser[JavaToken] = "/" ^^ { _ => Op_Slash }
  def p_Op_And:Parser[JavaToken] = "&" ^^ { _ => Op_And }
  def p_Op_Or:Parser[JavaToken] = "|" ^^ { _ => Op_Or }
  def p_Op_Caret:Parser[JavaToken] = "^" ^^ { _ => Op_Caret } 
  def p_Op_Percent:Parser[JavaToken] = "%" ^^ { _ => Op_Percent }
  def p_Op_LShift:Parser[JavaToken] = "<<" ^^ { _ => Op_LShift }
  def p_Op_PlusE:Parser[JavaToken] = "+=" ^^ { _ => Op_PlusE }
  def p_Op_MinusE:Parser[JavaToken] = "-=" ^^ { _ => Op_MinusE }
  def p_Op_StarE:Parser[JavaToken] = "*=" ^^ { _ => Op_StarE }
  def p_Op_SlashE:Parser[JavaToken] = "/=" ^^ { _ => Op_SlashE } 
  def p_Op_AndE:Parser[JavaToken] = "&=" ^^ { _ => Op_AndE }
  def p_Op_OrE:Parser[JavaToken] = "|=" ^^ { _ => Op_OrE }
  def p_Op_CaretE:Parser[JavaToken] = "^=" ^^ { _ => Op_CaretE }
  def p_Op_PercentE:Parser[JavaToken] = "%=" ^^ { _ => Op_PercentE }
  def p_Op_LShiftE:Parser[JavaToken] = "<<=" ^^ { _ => Op_LShiftE }
  def p_Op_RShiftE:Parser[JavaToken] = ">>=" ^^ { _ => Op_RShiftE }
  def p_Op_RRShiftE:Parser[JavaToken] = ">>>=" ^^ { _ => Op_RRShiftE }
  def p_Op_AtSign:Parser[JavaToken] = "@" ^^ { _ => Op_AtSign }




  def parse_one(p:Parser[JavaToken], src: String): ParseResult[JavaToken] =
    parse(phrase(p), src)


  def tokens: Parser[List[JavaToken]] = phrase(
    rep1(
      p_ann_interface | 
      p_abstract | 
      p_assert | 
      p_boolean | 
      p_break | 
      p_byte | 
      p_case | 
      p_catch | 
      p_char | 
      p_class | 
      p_const | 
      p_continue | 
      p_default |
      p_do | 
      p_double |
      p_else | 
      p_enum | 
      p_extends | 
      p_final |
      p_finally |
      p_float |
      p_for |
      p_goto | 
      p_if | 
      p_implements | 
      p_import | 
      p_instanceof | 
      p_int | 
      p_interface | 
      p_long | 
      p_native | 
      p_new | 
      p_package | 
      p_private | 
      p_protected | 
      p_public | 
      p_return |
      p_short |
      p_static | 
      p_strictfp | 
      p_super | 
      p_switch | 
      p_synchronized |
      p_this |
      p_throw | 
      p_throws | 
      p_transient |
      p_try | 
      p_void |
      p_volatile | 
      p_while |
      p_IntTok | 
      p_LongTok | 
      p_DoubleTok |
      p_FloatTok |
      p_BoolTok |
      p_CharTok |
      p_StringTok | 
      p_NullTok | 
      p_IdentTok | 
      p_OpenParen | 
      p_CloseParen | 
      p_OpenSquare |
      p_CloseSquare |
      p_OpenCurly | 
      p_CloseCurly | 
      p_SemiColon | 
      p_Comma | 
      p_Period |
      p_LambdaArrow |
      p_MethodRefSep |
      p_Op_Equal | 
      p_Op_GThan | 
      p_Op_LThan | 
      p_Op_Bang | 
      p_Op_Tilde | 
      p_Op_Query | 
      p_Op_Colon | 
      p_Op_Equals | 
      p_Op_LThanE | 
      p_Op_GThanE | 
      p_Op_BangE |
      p_Op_AAnd |
      p_Op_OOr | 
      p_Op_PPlus |
      p_Op_MMinus | 
      p_Op_Plus | 
      p_Op_Minus | 
      p_Op_Star | 
      p_Op_Slash | 
      p_Op_And | 
      p_Op_Or | 
      p_Op_Caret | 
      p_Op_Percent | 
      p_Op_LShift | 
      p_Op_PlusE | 
      p_Op_MinusE |
      p_Op_StarE | 
      p_Op_SlashE | 
      p_Op_AndE |
      p_Op_OrE |
      p_Op_CaretE | 
      p_Op_PercentE | 
      p_Op_LShiftE | 
      p_Op_RShiftE |
      p_Op_RRShiftE | 
      p_Op_AtSign 
    )
  )

  def tokenize(src: String): ParseResult[List[JavaToken]] =
    parse(tokens, src)

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
