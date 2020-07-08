package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj.Lexer._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj._
// import cats.implicits._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
// import lexical.StdLexical

object Parser extends Parsers {
    override type Elem = JavaToken 
    // val lexical: StdLexical = new StdLexical()


    def lit:Parser[Literal] = accept("literal", {
        case IntTok(s,i)    => IntLit(i)
        case LongTok(s,i)   => LongLit(i)
        case DoubleTok(s,d) => DoubleLit(d)
        case FloatTok(s,f)  => FloatLit(f)
        case CharTok(s,c)   => CharLit(c)
        case StringTok(s, v) => StringLit(s)
        case BoolTok(s,b)   => BooleanLit(b)
        case NullTok(s)     => NullLit 
    })
    
    def parse(s:String) : Literal = {
        lit(new Lexer.Scanner(s)) match {
            case Success(result, _) => result
        }
    }
    

}

