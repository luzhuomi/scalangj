package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Token._

import org.scalatest.{FunSuite, Matchers}
class TestAnnInterfaceLexer extends FunSuite with Matchers {
  val STRING = "@interface"
  val TOKEN = KW_AnnInterface

  test("@interface is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_ann_interface, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAbstrLexer extends FunSuite with Matchers {
  val STRING = "abstract"
  val TOKEN = KW_Abstract

  test("abstract is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_abstract, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestAssert extends FunSuite with Matchers {
  val STRING = "assert"
  val TOKEN = KW_Assert

  test("assert is lexed correlectly") {
    val result = Lexer.parse_one(Lexer.p_assert, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }

}

class TestInt1 extends FunSuite with Matchers {
  val STRING = "1234"
  val TOKEN = IntTok(1234)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestInt2 extends FunSuite with Matchers {
  val STRING = "0x1A1"
  val TOKEN = IntTok(417)
  test(s"int token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IntTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong1 extends FunSuite with Matchers {
  val STRING = "1234L"
  val TOKEN = LongTok(1234)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLong2 extends FunSuite with Matchers {
  val STRING = "0x1A1l"
  val TOKEN = LongTok(417)
  test(s"long token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_LongTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok1 extends FunSuite with Matchers {
  val STRING = "'A'"
  val TOKEN = CharTok('A')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))

  }
}

class TestCharTok2 extends FunSuite with Matchers {
  val STRING = "'\t'"
  val TOKEN = CharTok('\t')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestCharTok3 extends FunSuite with Matchers {
  val STRING = "'\t'"
  val TOKEN = CharTok('t')
  test(s"char token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_CharTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestStringTok1 extends FunSuite with Matchers {
  val STRING = """"hello world!""""
  val TOKEN = StringTok("hello world!")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestStringTok2 extends FunSuite with Matchers {
  val STRING = """"hello world!""""
  val TOKEN = StringTok("hello world")
  test(s"string token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_StringTok, STRING)
    assert((result.successful) && (result.get !== TOKEN))
  }
}

class TestIdentTok1 extends FunSuite with Matchers {
  val STRING = "myAddr_2"
  val TOKEN = IdentTok("myAddr_2")
  test(s"identifier token ${STRING} is lexed correctly") {
    val result = Lexer.parse_one(Lexer.p_IdentTok, STRING)
    assert((result.successful) && (result.get === TOKEN))
  }
}

class TestLexer1 extends FunSuite with Matchers {
  val STRING = "int x = 1;"
  val TOKENS = List(KW_Int, IdentTok("x"), Op_Equal, IntTok(1), SemiColon)
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
  val TOKENS = List(KW_Public, KW_Class, IdentTok("HelloWorld"), OpenCurly,
  KW_Public, KW_Static, KW_Void, IdentTok("main"), OpenParen, CloseParen, OpenCurly,
  IdentTok("System"), Period, IdentTok("out"), Period, IdentTok("println"), OpenParen, StringTok("Hello World!"), CloseParen, SemiColon,
  CloseCurly,
  CloseCurly 
  )
  test(s"phrase ${STRING} is lexed correctly") {
    val result = Lexer.tokenize(STRING)
    assert((result.successful) && (result.get === TOKENS))
  }
}
