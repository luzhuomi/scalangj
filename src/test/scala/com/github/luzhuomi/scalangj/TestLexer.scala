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

class TestAnnInterfaceLexer extends FunSuite with Matchers {
  val ANN_IFACE_STRING = "@interface"
  val ANN_IFACE_TOKEN = KW_AnnInterface

  test("@interface is lexed correlectly") {
    Lexer.parse_one(Lexer.ann_interface,ANN_IFACE_STRING).map(token => token shouldBe ANN_IFACE_TOKEN )
  }
}