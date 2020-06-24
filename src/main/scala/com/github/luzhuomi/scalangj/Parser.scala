package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj.Lexer._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Token._
import cats.implicits._
import scala.util.parsing.combinator._


object Parser extends Parsers {
    override type Elem = JavaToken 



    def lit:Parser[Literal] = accept("literal", {
        case IntTok(i)    => IntLit(i)
        case LongTok(i)   => LongLit(i)
        case DoubleTok(d) => DoubleLit(d)
        case FloatTok(f)  => FloatLit(f)
        case CharTok(c)   => CharLit(c)
        case StringTok(s) => StringLit(s)
        case BoolTok(b)   => BooleanLit(b)
        case NullTok      => NullLit 
    })

}

