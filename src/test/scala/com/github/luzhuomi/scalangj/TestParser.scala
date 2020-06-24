package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Token._
import com.github.luzhuomi.scalangj.Syntax._

import org.scalatest.{FunSuite, Matchers}


// need to add a scanner to the lexer to produce Reader[JavaToken]
/*
class TestLitParser extends FunSuite with Matchers {
  val STRING = "1"
  val TOKEN = IntTok(1)
  val LITERAL = IntLit(1)
  test("@interface is lexed correlectly") {
    val result = Parser.lit.apply(List(TOKEN))
    assert((result.successful) && (result.get === LITERAL))
  }
}
*/