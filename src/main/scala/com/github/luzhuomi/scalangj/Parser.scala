package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj.Lexer._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj._
// import cats.implicits._
import scala.util.parsing.combinator._


object Parser extends Parsers {
    override type Elem = JavaToken 



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

}

