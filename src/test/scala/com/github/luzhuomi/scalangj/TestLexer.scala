package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import org.scalatest.{FunSuite, Matchers}

/*
class TestFormulaLexer extends FunSuite with Matchers {
  val SIMPLE_FORMULA = "($1 + $2) / ($3 - $4) + 100 * $5"
  val SIMPLE_FORMULA_TOKENS = List(
    BRACE_LEFT, VARIABLE(1), OPERATOR_ADD, VARIABLE(2), BRACE_RIGHT, OPERATOR_DIVIDE,
    BRACE_LEFT, VARIABLE(3), OPERATOR_SUBTRACT, VARIABLE(4), BRACE_RIGHT,
    OPERATOR_ADD, CONSTANT(100.0), OPERATOR_MULTIPLY, VARIABLE(5)
  )

  test("formula is tokenized correctly") {
    FormulaLexer
      .tokenize(SIMPLE_FORMULA)
      .map(tokens => tokens shouldBe SIMPLE_FORMULA_TOKENS)
  }
}

 */


/*

class TestAnnInterfaceLexer extends FunSuite with Matchers {
  val STRING = "@interface"
  val TOKEN = KW_AnnInterface

  test("@interface is lexed correlectly") {
    Lexer
      .parse_one(Lexer.p_ann_interface, STRING)
      .map(token => token shouldBe TOKEN)
  }
}

class TestAbstrLexer extends FunSuite with Matchers {
  val STRING = "abstract"
  val TOKEN = KW_Abstract

  test("abstract is lexed correlectly") {
    Lexer.parse_one(Lexer.p_abstract, STRING).map(token => token shouldBe TOKEN)
  }
}

class TestAssert extends FunSuite with Matchers {
  val STRING = "assert"
  val TOKEN = KW_Assert

  test("assert is lexed correlectly") {
    Lexer.parse_one(Lexer.p_assert, STRING).map(token => token shouldBe TOKEN)
  }

}


class TestInt1 extends FunSuite with Matchers {
  val STRING = "1234"
  val TOKEN = IntTok(1234)
  test(s"int token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_IntTok, STRING).map(token => token shouldBe TOKEN)
  }
}


class TestInt2 extends FunSuite with Matchers {
  val STRING = "0x1A1"
  val TOKEN = IntTok(417)
  test(s"int token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_IntTok, STRING).map(token => token shouldBe TOKEN)
  }
}



class TestLong1 extends FunSuite with Matchers {
  val STRING = "1234L"
  val TOKEN = LongTok(1234)
  test(s"long token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_LongTok, STRING).map(token => token shouldBe TOKEN)
  }
}


class TestLong2 extends FunSuite with Matchers {
  val STRING = "0x1A1l"
  val TOKEN = LongTok(417)
  test(s"long token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_LongTok, STRING).map(token => token shouldBe TOKEN)
  }
}


class TestCharTok1 extends FunSuite with Matchers {
  val STRING = "'A'"
  val TOKEN = CharTok('A')
  test(s"char token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_CharTok, STRING).map(token => token shouldBe TOKEN)
  }
}
*/


class TestCharTok2 extends FunSuite with Matchers {
  val STRING = "'\t'"
  val TOKEN = StringTok("t")
  test(s"char token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_CharTok, STRING).map(token => {
      
      assert(token == TOKEN)
    })
  }
}


/*
class TestStringTok extends FunSuite with Matchers {
  val STRING = "hello world!"
  val TOKEN = StringTok("hello world!")
  test(s"string token ${STRING} is lexed correctly") {
    Lexer.parse_one(Lexer.p_StringTok, STRING).map(token => assert(token == TOKEN))
  }
}
*/