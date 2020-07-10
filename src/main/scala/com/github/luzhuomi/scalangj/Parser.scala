package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj.Lexer._
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj._
// import cats.implicits._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
// import lexical.StdLexical
import scala.util.parsing.input._
import scala.language.implicitConversions

object Parser extends Parsers {
    override type Elem = JavaToken

    def parseCompilationUnit(s:String): ParseResult[CompilationUnit] = {
        compilationUnit(new Lexer.Scanner(s)) 
    }

    def compilationUnit: Parser[CompilationUnit] = {
        opt(packageDecl) ~ rep(importDecl) ~ rep(typeDecl) ~ EOF ^^ { 
            case mpd ~ ids ~ tds ~ _ => CompilationUnit(mpd, ids, tds.flatMap(x => x)) 
        }
    }

    def packageDecl:Parser[PackageDecl] = { 
        tok(KW_Package("package")) ~ name ~ semiColon ^^  {
            case pkg ~ n ~ _ => PackageDecl(n)
        }    
     }
    
    def importDecl:Parser[ImportDecl] = {
        tok(KW_Package("import")) ~ bopt(tok(KW_Static("static"))) ~ name ~ bopt(period ~> tok(Op_Star("*"))) ~ semiColon ^^ {
            case imp ~ st ~ n ~ ds ~ semiCol => ImportDecl(st,n,ds)
        }
    }

    def typeDecl:Parser[Option[TypeDecl]] = { 
        def someDecls:Parser[Option[TypeDecl]] = {
            classOrInterfaceDecl ^^ { decl => Some(decl) }
        }

        def noDecl:Parser[Option[TypeDecl]] = {
            semiColon ^^ { _ => None }
        }
        someDecls | noDecl
    }

    //----------------------------------------------------------------------------
    // -- Declarations
    // -- Class declarations
    def classOrInterfaceDecl:Parser[TypeDecl] = {
        rep(modifier) ~ (classDecl_Internal | interfaceDecl_Internal) ^^ {
            case (md ~ de) => de(md)
        }
    }

    def classDecl_Internal:Parser[Mod[TypeDecl]] = {
        classDecl ^^ { (cd:Mod[ClassDecl]) => ((ms:List[Modifier]) => ClassTypeDecl(cd(ms))) }        
    }

    def interfaceDecl_Internal:Parser[Mod[TypeDecl]] = {
        (annIntefaceDecl | interfaceDecl) ^^  { (id:Mod[InterfaceDecl]) => ((ms:List[Modifier]) => InterfaceTypeDecl(id(ms)))}
    }

    type Mod[A] = List[Modifier] => A

    def classDecl:Parser[Mod[ClassDecl]] = normalClassDecl | enumClassDecl

    def normalClassDecl:Parser[Mod[ClassDecl]] = {
        tok(KW_Class("class")) ~ ident ~ lopt(typeParams) ~ opt(exts) ~ lopt(impls) ~ classBody ^^ {
            case (_ ~ i ~ tps ~ mex ~ imp ~ bod)  => ((ms:List[Modifier]) => ClassDecl_(ms, i, tps, mex.map(_.head), imp, bod))
        }
    }

    def exts:Parser[List[RefType]] = { 
        tok(KW_Extends("extends")) ~> refTypeList ^^ { rts => rts }
    }


    def impls:Parser[List[RefType]] = { 
        tok(KW_Implements("implements")) ~> refTypeList ^^ { rts => rts }
    }

    def enumClassDecl:Parser[Mod[ClassDecl]] = {
        tok(KW_Enum("enum")) ~ ident ~ lopt(impls) ~ enumBody ^^ {
            case (_ ~ i ~ imp ~ bod) => {
                ms => EnumDecl(ms, i, imp, bod)
            }
        }
    }


    def classBody:Parser[ClassBody] = {
        braces(classBodyStatements) ^^ { b => ClassBody(b)}
    }

    def enumBody:Parser[EnumBody] = {
        def inner:Parser[EnumBody] = {
            seplist(enumConst, comma) ~ opt(comma) ~ lopt(enumBodyDecls) ^^ {
                case ecs ~ _ ~ eds => EnumBody(ecs,eds)
            }
        }
        braces(inner)
    } 

    def enumConst:Parser[EnumConstant] = {
        ident ~ lopt(args) ~ opt(classBody) ^^ { 
            case (id ~ ars ~ mcb) => {
                EnumConstant(id, ars, mcb)
            }
        }
    }

    def enumBodyDecls:Parser[List[Decl]] = semiColon ~> classBodyStatements

    def classBodyStatements:Parser[List[Decl]] = {
        list(classBodyStatement) ^^ { mcbs => mcbs.flatMap(x => x) }
    }

    
    def annIntefaceDecl:Parser[Mod[InterfaceDecl]] = {
        failure("TODO")
    }

    def interfaceDecl:Parser[Mod[InterfaceDecl]] = {
        failure("TODO")
    }

    def classBodyStatement:Parser[Option[Decl]] = failure("TODO")

    def modifier:Parser[Modifier] = {
        failure("TODO")
    }


    def refType:Parser[RefType] = failure("TODO")
    

    def refTypeList:Parser[List[RefType]] = seplist1(refType,comma)

    // --------------------------------------------------------------------------------
    // Type parameters and arguments

    def typeParams:Parser[List[TypeParam]] = failure("TODO")


    def args:Parser[List[Argument]] = parens(seplist(exp,comma))


    def exp:Parser[Exp] = assignExp 


    // note scala parsec | is default backtracking operator, with explicit commit
    def assignExp:Parser[Exp] = methodRef | lambdaExp | assignment | condExp

    def condExp:Parser[Exp] = failure("TODO")

    def lambdaExp:Parser[Exp] = failure("TODO")

    def assignment:Parser[Exp] = failure("TODO")

    def methodRef:Parser[Exp] = failure("TODO")

    def literal: Parser[Literal] = 
    accept(
        "literal", {
            case IntTok(s, i)    => IntLit(i)
            case LongTok(s, i)   => LongLit(i)
            case DoubleTok(s, d) => DoubleLit(d)
            case FloatTok(s, f)  => FloatLit(f)
            case CharTok(s, c)   => CharLit(c)
            case StringTok(s, v) => StringLit(s)
            case BoolTok(s, b)   => BooleanLit(b)
            case NullTok(s)      => NullLit
        }
    )
    /*
    def parseLiteral(s: String): Literal = {
        lit(new Lexer.Scanner(s)) match {
            case Success(result, _) => result
        }
    }
    */
    def parseLiteral(s: String): ParseResult[Literal] = {
        literal(new Lexer.Scanner(s)) 
    }


    // ------------------------------------------------------------------
    // Names

    def name:Parser[Name] = {
        seplist1(ident,period) ^^ { ids => Name(ids) }
    }

    def ident:Parser[Ident] = {
        accept("identifier", {
            case IdentTok(s) => Ident(s)
        })
    }

    // ------------------------------------------------------------------


    def comma:Parser[JavaToken] = tok(Comma(","))
    def period:Parser[JavaToken] = tok(Period("."))
    def semiColon:Parser[JavaToken] = tok(SemiColon(";"))

    def tok(e:Elem):Parser[Elem] = elem(e)

    def bopt[A](p:Parser[A]):Parser[Boolean] = {
        opt(p) ^^ { mb => mb match {
            case Some(a) => true
            case None => false
        }}
    }

    def lopt[A](p:Parser[List[A]]):Parser[List[A]] = {
        opt(p) ^^ { mas => mas match {
            case None => List()
            case Some(as) => as
        }}
    }

    def list[A](p:Parser[A]):Parser[List[A]] = {
        val e:List[A] = Nil
        option(e,list1(p))
    }

    def list1[A](p:Parser[A]):Parser[List[A]] = rep1(p)

    def seplist[A,SEP](p:Parser[A], sep:Parser[SEP]):Parser[List[A]] = {
        val e:List[A] = Nil
        option(e, seplist1(p,sep))
    }
    def seplist1[A,SEP](p:Parser[A], sep:Parser[SEP]):Parser[List[A]] = {
        p ~ seplist(p, sep) ^^ { case a ~ as => a::as }
    }

    def option[A](a:A,p:Parser[A]):Parser[A] = p | success(a)


    def between[O,A,C](open:Parser[O], close:Parser[C], p:Parser[A]):Parser[A] = {
        open ~ p ~ close ^^ { case o ~ a ~ c => a }
    }

    def parens[A](p:Parser[A]):Parser[A] = between(tok(OpenParen("(")),tok(CloseParen(")")), p)

    def braces[A](p:Parser[A]):Parser[A] = between(tok(OpenCurly("{")),tok(CloseCurly("}")), p)

    def brackets[A](p:Parser[A]):Parser[A] = between(tok(OpenSquare("[")),tok(CloseSquare("]")), p)

    def angles[A](p:Parser[A]):Parser[A] = between(tok(Op_LThan("<")),tok(Op_GThan(">")), p)


}
