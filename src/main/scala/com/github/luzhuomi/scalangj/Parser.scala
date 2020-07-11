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
import java.security.KeyStore.TrustedCertificateEntry

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
        (annInterfaceDecl | interfaceDecl) ^^  { (id:Mod[InterfaceDecl]) => ((ms:List[Modifier]) => InterfaceTypeDecl(id(ms)))}
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


    def annInterfaceDecl:Parser[Mod[InterfaceDecl]] = {
        tok(KW_AnnInterface("@interface")) ~ ident ~ lopt(typeParams) ~ lopt(exts) ~ interfaceBody ^^ {
            case (_ ~ id ~ tps ~ exs ~ bod) => { ms => InterfaceDecl(InterfaceAnnotation, ms, id, tps, exs, bod)}
        }
    }

    def interfaceDecl:Parser[Mod[InterfaceDecl]] = {
        tok(KW_Interface("interface")) ~ ident ~ lopt(typeParams) ~ lopt(exts) ~ interfaceBody ^^ {
            case (_ ~ id ~ tps ~ exs ~ bod) => { ms => InterfaceDecl(InterfaceNormal, ms, id, tps, exs, bod)}
        }
    }

    def interfaceBody:Parser[InterfaceBody] = {
        braces(list(interfaceBodyDecl)) ^^ { decls => InterfaceBody(decls.flatMap(x => x))}
    }

    // Declarations 

    def classBodyStatement:Parser[Option[Decl]] = {
        def pNothing:Parser[Option[Decl]] = {
            list1(semiColon) ^^ { x => None }
        }
        def pInit:Parser[Option[Decl]] = {
            bopt(tok(KW_Static("static"))) ~ block ^^ {
                case ( mst ~ blk) => Some(InitDecl(mst,blk))
            }
        }
        def pMember:Parser[Option[Decl]] = {
            list(modifier) ~ memberDecl ^^ {
                case ( ms ~ dec ) => Some(MemberDecl_(dec(ms)))
            }
        }
        pNothing | pInit | pMember
    }

    def memberDecl:Parser[Mod[MemberDecl]] = {
        def mClassDecl:Parser[Mod[MemberDecl]] = {
            classDecl ^^ { cd => ms => MemberClassDecl(cd(ms))}
        }

        def mInterfaceDecl:Parser[Mod[MemberDecl]] = {
            (annInterfaceDecl | interfaceDecl) ^^ { id => ms => MemberInterfaceDecl(id(ms))} 
        }

        mClassDecl | mInterfaceDecl | fieldDecl | methodDecl | constrDecl
    }

    def fieldDecl:Parser[Mod[MemberDecl]] = {
        def p = {
            ttype ~ varDecls ^^ {
                case typ ~ vds => { ms => FieldDecl(ms,typ,vds)}
            }
        }
        endSemi(p)
    }

    def methodDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ resultType ~ ident ~ formalParams ~ lopt(throws) ~ methodBody ^^ {
            case (tps ~ rt ~ id ~ fps ~ thr ~ bod ) => {
                ms => MethodDecl(ms,tps,rt,id,fps,thr,None,bod)
            }
        }
    }

    def methodBody:Parser[MethodBody] = {
        def pNothing = {
            semiColon ^^ { _ => None }
        }
        def pSome = {
            block ^^ { b => Some(b) }
        }
        (pNothing | pSome) ^^ {
            mb => MethodBody(mb)
        }
    }

    def constrDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ ident ~ formalParams ~ lopt(throws) ~ constrBody ^^ {
            case (tps ~ id ~ fps ~ thr ~ bod) => {
                ms => ConstructorDecl(ms,tps,id,fps,thr,bod)
            }
        }
    }

    def constrBody:Parser[ConstructorBody] = {
        def p = {
            opt(explConstrInv) ~ list(blockStmt) ^^ {
                case mec ~ bss => { ConstructorBody(mec, bss) }
            }
        }
        braces(p)
    }

    def explConstrInv:Parser[ExplConstrInv] = {
        def p1 = {
            lopt(refTypeArgs) ~ tok(KW_This("this")) ~ args ^^ {
                case tas ~ _ ~ arguments => ThisInvoke(tas,arguments)
            }
        }
        def p2 = {
            lopt(refTypeArgs) ~ tok(KW_Super("super")) ~ args ^^ {
                case tas ~ _ ~ arguments => SuperInvoke(tas,arguments)
            }
        }
        def p3 = {
            primary ~ period ~ lopt(refTypeArgs) ~ tok(KW_Super("super")) ~ args ^^ {
                case pri ~ _ ~ tas ~ _ ~ arguments => PrimarySuperInvoke(pri, tas, arguments) 
            }
        }
        endSemi(p1|p2|p3)
    }

    //  TODO: This should be parsed like class bodies, and post-checked.
    //       That would give far better error messages.
    def interfaceBodyDecl:Parser[Option[MemberDecl]] = {
        def pNone = semiColon ^^ { _ => None }
        def pSome = list(modifier) ~ interfaceMemberDecl ^^ { case ms ~ imd => Some(imd(ms)) }
        pNone | pSome
    }

    def interfaceMemberDecl:Parser[Mod[MemberDecl]] = {
        def p1 = classDecl ^^ { cd => { (ms:List[Modifier]) => MemberClassDecl(cd(ms))}} 
        def p2 = (annInterfaceDecl | interfaceDecl) ^^ { id => { (ms:List[Modifier]) => MemberInterfaceDecl(id(ms))}} 
        p1 | p2 | fieldDecl | absMethodDecl 
    }

    def absMethodDecl:Parser[Mod[MemberDecl]] = {
        lopt(typeParams) ~ resultType ~ ident ~ formalParams ~ lopt(throws) ~ opt(defaultValue) ~ semiColon ^^ {
            case tps ~ rt ~ id ~ fps ~ thr ~ defv ~ _ => { ms => MethodDecl(ms,tps,rt,id,fps,thr,defv, (MethodBody(None)))}
        }
    }

    def defaultValue:Parser[Exp] = tok(KW_Default("default")) ~> exp


    // Formal parameters

    def formalParams:Parser[List[FormalParam]] = {
        def validateFPs(fps:List[FormalParam]):Boolean = fps match 
        {
            case Nil => true 
            case (_::Nil) => true 
            case (FormalParam(_,_,b,_)::xs) => !b
        }
        def p:Parser[List[FormalParam]] = {
            seplist(formalParam,comma) >> { 
                fps => if (validateFPs(fps)) {
                    success(fps)
                } else {
                    failure("Only the last formal parameter may be of variable arity")
                }
            }
        }
        parens(p)
    }

    def formalParam:Parser[FormalParam] = {
        list(modifier) ~ ttype ~ bopt(ellipsis) ~ varDeclId ^^ {
            case ms ~ typ ~ vr ~ vid => FormalParam(ms, typ, vr, vid)
        }
    }

    def ellipsis:Parser[Unit] = {
        period ~> period ~> period ^^ { _ => () }
    }

    def modifier:Parser[Modifier] = {
        def p_public = tok(KW_Public("public")) ^^ { _ => Public }
        def p_protected = tok(KW_Protected("protected")) ^^ { _ => Protected }
        def p_private = tok(KW_Private("private")) ^^ { _ => Private } 
        def p_abstract = tok(KW_Abstract("abstract")) ^^ { _ => Abstract }
        def p_static = tok(KW_Static("static")) ^^ { _ => Static }
        def p_strictfp = tok(KW_Strictfp("strictfp")) ^^ { _ => StrictFP }
        def p_final = tok(KW_Final("final")) ^^ { _ => Final } 
        def p_native = tok(KW_Native("native")) ^^ { _ => Native }
        def p_transient = tok(KW_Transient("transient")) ^^ { _ => Transient }
        def p_volatile = tok(KW_Volatile("volatile")) ^^ { _ => Volatile }
        def p_synchronized = tok(KW_Synchronized("synchronized")) ^^ { _ => Synchronized }
        def p_annotation = annotation ^^ { ann => Annotation_(ann) }

        p_public | p_protected | p_private | p_abstract | p_static | p_strictfp | p_final | p_final | p_native | p_transient | p_volatile | p_synchronized | p_annotation
    }

    def annotation:Parser[Annotation] = {
        def normAnn = {
            parens(evlist) ^^ { ps => { n => NormalAnnotation(n,ps)}}
        }
        def singleElemAnn = {
            parens(elementValue) ^^ { p => { n => SingleElementAnnotation(n,p)} }
        } 
        def markerAnn = {
            success(()) ^^ { _ => {n => MarkerAnnotation(n)}}
        }

        tok(Op_AtSign("@")) ~ name ~ (normAnn|singleElemAnn|markerAnn) ^^ { 
            case _ ~ n ~ c => {c(n)}
        }
    }

    def evlist:Parser[List[(Ident,ElementValue)]] = {
        seplist1(elementValuePair, comma)
    }

    def elementValuePair:Parser[(Ident, ElementValue)] = {
        ident ~ tok(Op_Equal("=")) ~ elementValue ^^ {
            case (id ~ _ ~ ev) => (id,ev)
        }
    }

    def elementValue:Parser[ElementValue] = {
        def arrInit = { arrayInit ^^ { ai => InitArray(ai)}}
        def expInit = { condExp ^^ { e => InitExp(e)}}
        def evval = (arrInit | expInit) ^^ ( vi => EVVal(vi)) 
        def evann = annotation ^^ { ann => EVAnn(ann) }
        evval | evann
    }

    def throws:Parser[List[RefType]] = {
        tok(KW_Throws("throws")) ~> refTypeList
    }

    
    // ------------------------------------------------------------------
    // Variable declarations

    def varDecls:Parser[List[VarDecl]] = seplist1(varDecl,comma)

    def varDecl:Parser[VarDecl] = {
        varDeclId ~ opt(tok(Op_Equal("=")) ~> varInit) ^^ {
            case vid ~ mvi => VarDecl(vid,mvi)
        }
    }

    def varDeclId:Parser[VarDeclId] = {
        ident ~ list(arrBrackets) ^^ {
            case id ~ abs => {
                val vid:VarDeclId = VarId(id)
                abs.foldLeft(vid)((f:VarDeclId,_) => VarDeclArray(f))
            }
        }
    }

    def arrBrackets:Parser[Unit] = brackets(success(()))

    def localVarDecl:Parser[(List[Modifier], Type, List[VarDecl])] = {
        list(modifier) ~ ttype ~ varDecls ^^ { 
            case ms ~ typ ~ vds => (ms, typ, vds)
        }
    }

    def varInit:Parser[VarInit] = {
        def arrInit = arrayInit ^^ { ai => InitArray(ai) }
        def expInit = exp ^^ { e => InitExp(e) }
        arrInit | expInit
    }

    def arrayInit:Parser[ArrayInit] = {
        def p:Parser[ArrayInit] = {
            seplist(varInit,comma) ~ opt(comma) ^^ { case (vis ~ o) => ArrayInit(vis) }
        }
        braces(p)
    }

    // --------------------------------------------------------------------------------
    // Statements

    def block:Parser[Block] = {
        def blk = list(blockStmt) ^^ { bs => Block(bs) }
        braces(blk)
    }

    def blockStmt:Parser[BlockStmt] = {
        def localClass:Parser[BlockStmt] = {
            list(modifier) ~ classDecl ^^ { case ms ~ cd => LocalClass(cd(ms)) }
        }
        def localVars:Parser[BlockStmt] = {
            endSemi(localVarDecl) ^^ { case (m,t,vds) => LocalVars(m,t,vds) }
        }
        def pStmt:Parser[BlockStmt] = stmt ^^ { s => BlockStmt_(s) }
        localClass | localVars | pStmt
    }

    def stmt:Parser[Stmt] = {
        def thEl = {
            stmtNSI ~ tok(KW_Else("else")) ~ stmt ^^ {
                case th ~ _ ~ el => { e => IfThenElse(e,th,el)}
            } 
        }
        def thN = {
            stmt ^^ { 
                case th => { e => IfThen(e,th )}
            }
        }
        def ifStmt = {
            tok(KW_If("if")) ~ parens(exp) ~ (thEl | thN) ^^ {
                case _ ~ e ~ th => {th(e)}
            }
        }

        def whileStmt = {
            tok(KW_While("while")) ~ parens(exp) ~ stmt ^^ {
                case _ ~ e ~ s => {While(e,s)}
            }
        }

        def basicFor = {
            opt(forInit) ~ semiColon ~ opt(exp) ~ semiColon ~ opt(forUp) ^^ {
                case fi ~ _ ~ e ~ _ ~ fu => { s =>  BasicFor(fi,e,fu,s) }
            }
        }

        def enhancedFor = {
            list(modifier) ~ ttype ~ ident ~ colon ~ exp ^^ {
                case ms ~ t ~ i ~ _ ~ e => { (s:Stmt) => EnhancedFor(ms,t,i,e,s)}
            }
        }

        def forStmt = {
            tok(KW_For("for")) ~ parens(basicFor|enhancedFor) ~ stmt ^^ {
                case _ ~ f ~ s => f(s)
            }
        }

        def labeledStmt = {
            ident ~ colon ~ stmt ^^ { case lbl ~ _ ~ s => Labeled(lbl,s) }
        }

        ifStmt | whileStmt | forStmt | labeledStmt | stmtNoTrail 
    }

    def stmtNSI:Parser[Stmt] = {
        def ifStmt = {
            tok(KW_If("if")) ~ parens(exp) ~ stmtNSI ~ tok(KW_Else("else")) ~ stmtNSI  ^^ {
                case _ ~ e ~ th ~ _ ~ el => {IfThenElse(e,th,el)}
            }
        }
        def whileStmt = {
            tok(KW_While("while")) ~ parens(exp) ~ stmtNSI ^^ {
                case _ ~ e ~ s => {While(e,s)}
            }
        }

        def basicFor = {
            opt(forInit) ~ semiColon ~ opt(exp) ~ semiColon ~ opt(forUp) ^^ {
                case fi ~ _ ~ e ~ _ ~ fu => { s =>  BasicFor(fi,e,fu,s) }
            }
        }

        def enhancedFor = {
            list(modifier) ~ ttype ~ ident ~ colon ~ exp ^^ {
                case ms ~ t ~ i ~ _ ~ e => { (s:Stmt) => EnhancedFor(ms,t,i,e,s)}
            }
        }

        def forStmt = {
            tok(KW_For("for")) ~ parens(basicFor|enhancedFor) ~ stmtNSI ^^ {
                case _ ~ f ~ s => f(s)
            }
        }

        def labeledStmt = {
            ident ~ colon ~ stmtNSI ^^ { case lbl ~ _ ~ s => Labeled(lbl,s) }
        }
        ifStmt | whileStmt | forStmt | labeledStmt | stmtNoTrail 
    }
    def stmtNoTrail:Parser[Stmt] = {
        def empty = semiColon ^^ { _ => Empty }
        def stmtBlock = block ^^ { b => StmtBlock(b) }
        def assertStmt = endSemi({
            tok(KW_Assert("assert")) ~ exp ~ opt(colon ~> exp) ^^ {
                case _ ~ e ~ me2 => Assert(e,me2)
            }
        })
        def switchStmt = {
            tok(KW_Switch("switch")) ~ parens(exp) ~ switchBlock ^^ {
                case _ ~ e ~ sb => { Switch(e,sb) }
            }
        }
        def doWhile = endSemi({
            tok(KW_Do("do")) ~ stmt ~ tok(KW_While("while")) ~ parens(exp) ^^ {
                case _ ~ s ~ _ ~ e => { Do(s,e) }
            }
        })
        def breakStmt = endSemi({
            tok(KW_Break("break")) ~ opt(ident) ^^ {
                case _ ~ mi => Break(mi)
            }
        })
        def continueStmt = endSemi({
            tok(KW_Continue("continue")) ~ opt(ident) ^^ {
                case _ ~ mi => Continue(mi)
            }
        })
        def returnStmt = endSemi({
            tok(KW_Return("return")) ~ opt(exp) ^^ {
                case _ ~ me => Return(me) 
            }
        })
        def synchronizedStmt = {
            tok(KW_Synchronized("synchronized")) ~ parens(exp) ~ block ^^ {
                case _ ~ e ~ b => Synchronized(e,b) 
            }
        }
        def throwStmt = endSemi({
            tok(KW_Throw("throw")) ~ exp ^^ {
                case _ ~ e => Throw(e)
            }
        })
        def tryCatch = {
            tok(KW_Try("try")) ~ block ~ list(catchClause) ~ opt(tok(KW_Finally("finally")) ~> block) ^^ {
                case _ ~ b ~ c ~ mf => Try(b,c,mf)
            }  
        }
        def expStmt = endSemi({
            stmtExp ^^ { e => ExpStmt(e) }
        })
        empty | stmtBlock | assertStmt | switchStmt | doWhile | breakStmt | continueStmt | returnStmt | synchronizedStmt | throwStmt | tryCatch | expStmt
    }
    def forInit:Parser[ForInit] = failure("TODO")
    def forUp:Parser[List[Exp]] = seplist1(stmtExp,comma)

    def switchBlock:Parser[List[SwitchBlock]] = braces(list(switchStmt))

    def switchStmt:Parser[SwitchBlock] = {
        switchLabel ~ list(blockStmt) ^^ {
            case lbl ~ bss => SwitchBlock(lbl, bss)
        }
    }

    def switchLabel:Parser[SwitchLabel] = failure("TODO")
    
    def catchClause:Parser[Catch] = {
        tok(KW_Catch("catch")) ~ parens(formalParam) ~ block ^^ {
            case _ ~ fp ~ b => { Catch(fp, b) }
        }
    }
    def stmtExp:Parser[Exp] = failure("TODO")
    // --------------------------------------------------------------------------------
    // Type parameters and arguments

    def typeParams:Parser[List[TypeParam]] = angles(seplist1(typeParam,comma))

    def typeParam:Parser[TypeParam] = {
        ident ~ lopt(bounds) ^^ { case i ~ bs => TypeParam(i,bs) }
    }

    def bounds:Parser[List[RefType]] = tok(KW_Extends("extends")) ~> seplist1(refType, (tok(Op_And("&"))))

    def typeArgs:Parser[List[TypeArgument]] = angles(seplist1(typeArg,comma))


    def typeArg:Parser[TypeArgument] = {
        def wcTyArg:Parser[TypeArgument] = {
            tok(Op_Query("?")) ~> opt(wildcardBound) ^^ {
                owcb => Wildcard(owcb)
            }
        }
        def actTyArg:Parser[TypeArgument] = {
            refType ^^ {
                rt => ActualType(rt)
            }
        }
        wcTyArg | actTyArg
    }

    def wildcardBound:Parser[WildcardBound] = {
        def extendsBound:Parser[WildcardBound] = {
            tok(KW_Extends("extends")) ~> refType ^^ {
                rt => ExtendsBound(rt)
            }
        }

        def superBound:Parser[WildcardBound] = {
            tok(KW_Super("super")) ~> refType ^^ {
                rt => SuperBound(rt)
            }
        }
        extendsBound | superBound
    }

    def refTypeArgs:Parser[List[RefType]] = angles(refTypeList)


    def args:Parser[List[Argument]] = parens(seplist(exp,comma))


    def exp:Parser[Exp] = assignExp 


    // note scala parsec | is default backtracking operator, with explicit commit
    def assignExp:Parser[Exp] = methodRef | lambdaExp | assignment | condExp

    def condExp:Parser[Exp] = failure("TODO")

    def lambdaExp:Parser[Exp] = failure("TODO")

    def assignment:Parser[Exp] = failure("TODO")

    def methodRef:Parser[Exp] = failure("TODO")

    def primary:Parser[Exp] = failure("TODO")

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
    // Types

    def ttype:Parser[Type] = {
        def rType:Parser[Type] = refType ^^ {t => RefType_(t)}
        def pType:Parser[Type] = primType ^^ {t => PrimType_(t)}
        rType | pType
    }

    def primType:Parser[PrimType] = {
        def boolType:Parser[PrimType] = tok(KW_Boolean("boolean")) ^^ { _ => BooleanT }
        def byteType:Parser[PrimType] = tok(KW_Byte("byte")) ^^ { _ => ByteT }
        def shortType:Parser[PrimType] = tok(KW_Short("short")) ^^ { _ => ShortT }
        def intType:Parser[PrimType] = tok(KW_Int("int")) ^^ { _ => IntT }
        def longType:Parser[PrimType] = tok(KW_Long("long")) ^^ { _ => LongT }
        def charType:Parser[PrimType] = tok(KW_Char("char")) ^^ { _ => CharT }
        def floatType:Parser[PrimType] = tok(KW_Float("float")) ^^ { _ => FloatT }
        def doubleType:Parser[PrimType] = tok(KW_Double("double")) ^^ { _ => DoubleT }
        boolType | byteType | shortType | intType | longType | charType | floatType | doubleType 
    }

    def refType:Parser[RefType] = {
        def primArrType:Parser[RefType] = {
            primType ~ list1(arrBrackets) ^^ {
                case (pt ~ (_::bs)) =>
                {
                    val base:RefType = ArrayType(PrimType_(pt))
                    bs.foldLeft(base)((f,_) => ArrayType(RefType_(f)))
                    /* 
                    // this works
                    val arrTyConstr:(PrimType => RefType) = 
                        bs.foldLeft((t:PrimType)=>ArrayType(PrimType_(t)))((f,_) => t => ArrayType(RefType_(f(t))))
                    arrTyConstr(pt)
                    */
                }

            }
        }

        def clType:Parser[RefType] = {
            classType ~ list(arrBrackets) ^^ {
                case (ct ~ bs) => {
                    val base:RefType = ClassRefType(ct)
                    bs.foldLeft(base)((f,_) => ArrayType(RefType_(f)))
                    /*
                    // this does not type check in scala
                    val arrTyConstr:(ClassType => RefType) = 
                        bs.foldLeft((t:ClassType)=>ClassRefType(t))((f,_) => t => ArrayType(RefType_(f(t))))
                    arrTyConstr(ct)
                    */
                }
            }
        }
        (primArrType | clType).withErrorMessage("refType")
    }   

    def classType:Parser[ClassType] = seplist1(classTypeSpec,period) ^^ {
        ct => ClassType(ct)
    }

    def classTypeSpec:Parser[(Ident, List[TypeArgument])] = {
        ident ~ lopt(typeArgs) ^^ { 
            case i ~ tas  => (i, tas)
        }
    }

    def resultType:Parser[Option[Type]] = {
        def pNone:Parser[Option[Type]] = {
            tok(KW_Void("void")) ^^ { _ => None } 
        }
        def pSome:Parser[Option[Type]] = {
            ttype ^^ {
                t => Some(t)
            }
        }
        (pNone | pSome).withErrorMessage("resultType") 
    }

    def refTypeList:Parser[List[RefType]] = seplist1(refType,comma)






    // ------------------------------------------------------------------


    def comma:Parser[JavaToken] = tok(Comma(","))
    def period:Parser[JavaToken] = tok(Period("."))
    def semiColon:Parser[JavaToken] = tok(SemiColon(";"))
    def colon:Parser[JavaToken] = tok(Op_Colon(":"))

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

    def endSemi[A](p:Parser[A]):Parser[A] = p <~ semiColon
}
