package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Lexer._ // To unify the base class JavaToken, otherwise === will failed


import org.scalatest.{FunSuite, Matchers}
class TestAnnInterfaceLexer extends FunSuite with Matchers {
  val STRING = "@interface"
  val TOKEN:JavaToken = KW_AnnInterface(STRING)

  test("@interface is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_ann_interface, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAbstrLexer extends FunSuite with Matchers  {
  val STRING = "abstract"
  val TOKEN:JavaToken = KW_Abstract(STRING)

  test("abstract is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_abstract, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAssert extends FunSuite with Matchers  {
  val STRING = "assert"
  val TOKEN:JavaToken = KW_Assert(STRING)

  test("assert is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_assert, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }

}

class TestInt1 extends FunSuite with Matchers   {
  val STRING = "1234"
  val TOKEN:JavaToken  = IntTok(STRING,1234)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestInt2 extends FunSuite with Matchers  {
  val STRING = "0x1A1"
  val TOKEN:JavaToken  = IntTok(STRING, 417)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong1 extends FunSuite with Matchers {
  val STRING = "1234L"
  val TOKEN:JavaToken = LongTok(STRING,1234)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong2 extends FunSuite with Matchers {
  val STRING = "0x1A1l"
  val TOKEN:JavaToken  = LongTok(STRING, 417)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok1 extends FunSuite with Matchers {
  val STRING = "'A'"
  val TOKEN:JavaToken  = CharTok(STRING, 'A')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))

  }
}

class TestCharTok2 extends FunSuite with Matchers {
  val STRING = "'\t'"
  val TOKEN:JavaToken  = CharTok(STRING, '\t')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok3 extends FunSuite with Matchers {
  val STRING = "'\t'"
  val TOKEN:JavaToken  = CharTok("'t'", 't')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestStringTok1 extends FunSuite with Matchers {
  val STRING = """"hello world!""""
  val TOKEN:JavaToken  = StringTok(STRING, "hello world!")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestStringTok2 extends FunSuite with Matchers {
  val STRING = """"hello world!""""
  val TOKEN:JavaToken  = StringTok(""""hello world"""", "hello world")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestIdentTok1 extends FunSuite with Matchers {
  val STRING = "myAddr_2"
  val TOKEN:JavaToken  = IdentTok("myAddr_2")
  test(s"identifier token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IdentTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLexer1 extends FunSuite with Matchers {
  val STRING = "int x = 1;"
  val TOKENS:List[JavaToken] = List(KW_Int("int"), IdentTok("x"), Op_Equal("="), IntTok("1", 1), SemiColon(";"))
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}


class TestLexer2 extends FunSuite with Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
        System.out.println("Hello World!");
    }
} """
  val TOKENS:List[JavaToken] = List(KW_Public("public"), KW_Class("class"), IdentTok("HelloWorld"), OpenCurly("{"),
  KW_Public("public"), KW_Static("static"), KW_Void("void"), IdentTok("main"), OpenParen("("), CloseParen(")"), OpenCurly("{"),
  IdentTok("System"), Period("."), IdentTok("out"), Period("."), IdentTok("println"), OpenParen("("), StringTok(""""Hello World!"""", "Hello World!"), CloseParen(")"), SemiColon(";"),
  CloseCurly("}"),
  CloseCurly("}") 
  )
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}
