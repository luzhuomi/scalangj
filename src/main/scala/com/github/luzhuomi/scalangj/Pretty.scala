package com.github.luzhuomi.scalangj

import org.typelevel.paiges._
import org.typelevel.paiges.Doc._
import com.github.luzhuomi.scalangj.Syntax._
import cats.implicits._
import cats._
import scala.collection.immutable.IndexedSeq.Impl
import org.w3c.dom.Element



object Pretty {
    def prettyPrint[A](a:A)(implicit pa:Pretty[A]) : String = {
        ""
    }

    trait Pretty[A] {
        def pretty(a:A): Doc = prettyPrec(0,a)
        def prettyPrec(prec:Int, a:A):Doc = pretty(a)
 
    }
    
    
    implicit def compilationUnitPretty(implicit pdPty:Pretty[PackageDecl]
                                    , ipPty: Pretty[ImportDecl]
                                    , tdPty:Pretty[TypeDecl]) = new Pretty[CompilationUnit] { 
        override def prettyPrec(p:Int, cu:CompilationUnit) = cu match {
            case CompilationUnit(mpd, ids, tds) => {
                vcat( (maybePP(p,mpd)(pdPty)::ids.map(id=>ipPty.prettyPrec(p,id))) ++ tds.map(td => tdPty.prettyPrec(p,td)))
            }
        }
    }

    implicit def packageDeclPretty(implicit namePty:Pretty[Name]) = new Pretty[PackageDecl] {
        override def prettyPrec(p:Int, pdecl:PackageDecl) =  pdecl match {
            case PackageDecl(name) => 
                spread(List(text("package"), namePty.prettyPrec(p,name)))+semi 
        }
    }

    implicit def importDeclPretty(implicit namePty:Pretty[Name]) = new Pretty[ImportDecl]  {
        override def prettyPrec(p:Int, idecl:ImportDecl) = idecl match {
            case ImportDecl(st,name,wc) =>
                spread(List(text("import"), 
                            opt(st,text("static")), 
                            namePty.prettyPrec(p,name))) + opt(wc, text(".*")) + semi
        }
    }

    // ------------------------------------------------
    // Declarations

    implicit def typeDeclPretty(implicit cdPty:Pretty[ClassDecl]
                               , idPty:Pretty[InterfaceDecl]) = new Pretty[TypeDecl] {
        override def prettyPrec(p:Int, tdecl:TypeDecl) = tdecl match {
            case ClassTypeDecl(cd) => cdPty.prettyPrec(p,cd)
            case InterfaceTypeDecl(id) => idPty.prettyPrec(p,id)
        }
    }

    implicit def classDeclPretty(implicit modPty:Pretty[Modifier]
                                , idPty:Pretty[Ident]
                                , ebdPty:Pretty[EnumBody]
                                , cbdPty:Pretty[ClassBody]
                                , tpPty:Pretty[TypeParam]) = new Pretty[ClassDecl] {
        override def prettyPrec(p:Int, cdecl:ClassDecl) = cdecl match {
            case EnumDecl(mods,ident,impls,body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text("enum")
                val p3 = idPty.prettyPrec(p,ident)
                val p4 = ppImplements(p,impls)
                val p5 = ebdPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4)), p5))
            } 
            case ClassDecl_(mods,ident,tParams,mSuper,impls,body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text("class")
                val p3 = ppTypeParams(p,tParams)
                val p4 = ppExtends(p, maybe(Nil, (x:RefType)=>List(x), mSuper))
                val p5 = ppImplements(p,impls)
                val p6 = cbdPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4,p5)),p6))
            }
        }
    }

    implicit def classBodyPretty(implicit declPty:Pretty[Decl]) = new Pretty[ClassBody] {
        override def prettyPrec(p:Int, cbody:ClassBody) = cbody match {
            case ClassBody(ds) => braceBlock( ds.map(declPty.prettyPrec(p,_))) 
        }
    }

    implicit def enumBodyPretty(implicit ecPty:Pretty[EnumConstant], declPty:Pretty[Decl]) = new Pretty[EnumBody] {
        override def prettyPrec(p:Int, ebody:EnumBody) = ebody match {
            case EnumBody(cs,ds) => braceBlock( 
                punctuate(comma, cs.map(ecPty.prettyPrec(p, _))) ++ 
                (opt((ds.length > 0), semi)::(ds.map(declPty.prettyPrec(p, _))))
            )
        }
    }

    implicit def enumConstantPretty(implicit idPty:Pretty[Ident]
                                    , cbPty:Pretty[ClassBody]
                                    , arPty:Pretty[Argument]
                                    ) = new Pretty[EnumConstant] {
        override def prettyPrec(p:Int, ec:EnumConstant) = ec match {
            case EnumConstant(ident,args,mBody) => {
                stack(List(idPty.prettyPrec(p,ident) + opt(args.length > 0, ppArgs(p,args))
                         , maybePP(p,mBody))) 
            }
        }
    }

    implicit def interfaceDeclPretty(implicit modPty:Pretty[Modifier]
                                    , idPty:Pretty[Ident]
                                    , ifBodyPty:Pretty[InterfaceBody]
                                    , tpPty:Pretty[TypeParam]) = new Pretty[InterfaceDecl] 
    {
        override def prettyPrec(p:Int, ifd:InterfaceDecl) = ifd match {
            case InterfaceDecl(kind,mods, ident, tParams, impls, body) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = text(if (kind == InterfaceNormal) "interface" else "@interface")
                val p3 = idPty.prettyPrec(p,ident)
                val p4 = ppTypeParams(p,tParams)
                val p5 = ppExtends(p,impls)
                val p6 = ifBodyPty.prettyPrec(p,body)
                stack(List(hsep(List(p1,p2,p3,p4,p5)), p6))
            }
        }

    }

    implicit def interfaceBodyPretty(implicit mdPty:Pretty[MemberDecl]) = new Pretty[InterfaceBody] {
        override def prettyPrec(p:Int, ifb:InterfaceBody) = ifb match {
            case InterfaceBody(mds) => {
                braceBlock(mds.map(mdPty.prettyPrec(p,_)))
            }
        }
    }

    implicit def declPretty(implicit mdPty:Pretty[MemberDecl]
                            , blkPty:Pretty[Block] ) = new Pretty[Decl] {
        override def prettyPrec(p:Int, decl:Decl) = decl match {
            case MemberDecl_(md) => mdPty.prettyPrec(p,md)
            case InitDecl(b,bl) => {
                hsep(List(opt(b,text("static")), blkPty.prettyPrec(p,bl)))
            }
        }
    }

    implicit def memberDeclPretty(implicit cdPty:Pretty[ClassDecl]
                                , idPty:Pretty[InterfaceDecl]
                                , modPty:Pretty[Modifier]
                                , tyPty:Pretty[Type]
                                , vdPty:Pretty[VarDecl]
                                , identPty:Pretty[Ident]
                                , mbdyPty:Pretty[MethodBody]
                                , cbdyPty:Pretty[ConstructorBody]
                                , fParaPty:Pretty[FormalParam]
                                , tpPty:Pretty[TypeParam]) = new Pretty[MemberDecl] {
        override def prettyPrec(p:Int, mDecl:MemberDecl) =  mDecl match {
            case FieldDecl(mods,t,vds) => {
                val ppMods = mods.map(modPty.prettyPrec(p,_))
                val ppTy = tyPty.prettyPrec(p,t)
                val ppVdecl = vds.map(vdPty.prettyPrec(p,_))
                val ppVdecl_pun = punctuate(text(","),ppVdecl)
                hsep(ppMods ++ (ppTy::ppVdecl_pun)) + semi
            }
            case MethodDecl(mods, tParams, mt,ident, fParams, throws, deft, body) => {
                val ppMods = hsep(mods.map(modPty.prettyPrec(p,_)))
                val ppTyParam = ppTypeParams(p,tParams)
                val ppResTy = ppResultType(p,mt)
                val ppId = identPty.prettyPrec(p,ident)
                val ppAr = ppArgs(p,fParams)
                val ppTh = ppThrows(p,throws)
                val ppDef = ppDefault(p,deft)
                val ppBody = mbdyPty.prettyPrec(p,body)
                stack(List(hsep(List(ppMods, ppTyParam, ppResTy, ppId, ppAr, ppTh, ppDef)), ppBody))
            }
            case ConstructorDecl(mods, tParams, ident, fParams, throws, body) => {
                val ppMods = hsep(mods.map(modPty.prettyPrec(p,_)))
                val ppTyParam = ppTypeParams(p,tParams)
                val ppId = identPty.prettyPrec(p,ident)
                val ppAr = ppArgs(p,fParams)
                val ppTh = ppThrows(p,throws)
                val ppBody = cbdyPty.prettyPrec(p,body)
                stack(List(hsep(List(ppMods,ppTyParam, ppId, ppAr, ppTh)), ppBody))
            }
            case MemberClassDecl(cd) => cdPty.prettyPrec(p,cd)
            case MemberInterfaceDecl(id) => idPty.prettyPrec(p,id)
        }
    }

    implicit def varDeclPretty(implicit vdidPty:Pretty[VarDeclId]
                              ,viPty:Pretty[VarInit]
                              ) = new Pretty[VarDecl] {
        override def prettyPrec(p:Int, varDecl:VarDecl):Doc = varDecl match {
            case VarDecl(vdId,None) => vdidPty.prettyPrec(p,vdId)
            case VarDecl(vdId,Some(ie)) => {
                hsep(List(vdidPty.prettyPrec(p,vdId), char('='), viPty.prettyPrec(p,ie))) 
            }
        }
    }

    implicit def varDeclIdPretty(implicit idPty:Pretty[Ident]
                                ) = new Pretty[VarDeclId] {
        override def prettyPrec(p:Int, varDeclId:VarDeclId):Doc = varDeclId match {
            case VarId(ident) => idPty.prettyPrec(p,ident)
            case VarDeclArray(vId) => prettyPrec(p,vId)
        }
    }

    implicit def varInitPretty(implicit expPty:Pretty[Exp]) = new Pretty[VarInit] {
        override def prettyPrec(p:Int, varInit:VarInit):Doc = varInit match {
            case InitExp(e) => expPty.prettyPrec(p,e)
            case InitArray(ArrayInit(ai)) => {
                hsep(List(text("{"), hsep(punctuate(comma, ai.map(prettyPrec(p,_)))), text("}")))
            }
        }
    }

    implicit def formalParamPretty(implicit modPty:Pretty[Modifier]
                                , tyPty:Pretty[Type]
                                , vIdPty:Pretty[VarDeclId]) = new Pretty[FormalParam] {
        override def prettyPrec(p:Int, fParam:FormalParam):Doc = fParam match {
            case FormalParam(mods, t, b, vId) => {
                val p1 = hsep(mods.map(modPty.prettyPrec(p,_)))
                val p2 = tyPty.prettyPrec(p, t) + opt(b,text("..."))
                val p3 = vIdPty.prettyPrec(p, vId)
                hsep(List(p1,p2,p3))
            }
        }
    }
    implicit def methodBodyPretty(implicit blockPty:Pretty[Block]) = new Pretty[MethodBody] {
        override def prettyPrec(p:Int, mBody:MethodBody): Doc = mBody match {
            case MethodBody(mBlock) => maybe(semi, blockPty.prettyPrec(p,_), mBlock)

        }
    }
    implicit def constructorBodyPretty(implicit stmtPty:Pretty[BlockStmt]
                                    , eciPty:Pretty[ExplConstrInv]) = new Pretty[ConstructorBody] {
        override def prettyPrec(p:Int, cBody:ConstructorBody): Doc = cBody match {
            case ConstructorBody(mECI, stmts) => {
                braceBlock (maybePP(p, mECI)::stmts.map(stmtPty.prettyPrec(p,_)))
            }
        }
    }

    implicit def explConstrInvPretty(implicit rtPty:Pretty[RefType]
                                , argPty:Pretty[Argument] 
                                , expPty:Pretty[Exp]) = new Pretty[ExplConstrInv] {
        override def prettyPrec(p:Int, eci:ExplConstrInv) : Doc = eci match {
            case ThisInvoke(rts,args) => {
                hsep(List(ppTypeParams(p,rts), text("this") + ppArgs(p,args)(argPty) + semi))
            }
            case SuperInvoke(rts, args) => {
                hsep(List(ppTypeParams(p,rts), text("super") + ppArgs(p,args)(argPty) + semi))
            }
            case PrimarySuperInvoke(e, rts, args) => {
                val p1 = expPty.prettyPrec(p,e) + char(',')
                val p2 = hsep(List(ppTypeParams(p,rts), text("super") + ppArgs(p,args)(argPty) + semi))
                p1 + p2 
            }
        }
    }

    implicit def modifierPretty(implicit annPty:Pretty[Annotation]) = new Pretty[Modifier] {
        override def prettyPrec(p:Int, mod:Modifier):Doc = mod match {
            case Annotation_(ann) => stack(List(annPty.prettyPrec(p,ann), nest(-1, text(""))))
            case mod => text (mod.toString().toLowerCase())
        }
    }

    implicit def annotationPretty(implicit namePty:Pretty[Name]
                                , evPty:Pretty[ElementValue]
                                , idPty:Pretty[Ident]) = new Pretty[Annotation] {
        override def prettyPrec(p:Int, x:Annotation):Doc = x match {
            case MarkerAnnotation(annName) => text("@") + namePty.prettyPrec(p, annName) + text("")
            case SingleElementAnnotation(annName,annValue) => text("@") + namePty.prettyPrec(p, annName) + text("(") + evPty.prettyPrec(p,annValue) + text(")")
            case NormalAnnotation(annName,annKV) => text("@") + namePty.prettyPrec(p, annName) + text("(") + ppEVList(p, annKV) + text(")")
        }
    }

    
    def ppEVList(prec:Int, kvs:List[(Ident, ElementValue)])
            (implicit idPty:Pretty[Ident], vPty:Pretty[ElementValue]):Doc = {
        val ps = kvs.map( (p:(Ident,ElementValue)) => p match {
            case (k,v) => hsep(List(idPty.prettyPrec(prec,k), text("="), vPty.prettyPrec(prec,v) ))
        })
        hsep(punctuate(comma, ps))
    }

    implicit def elementValuePretty(implicit annPty:Pretty[Annotation]
                                    ,viPty:Pretty[VarInit] ) = new Pretty[ElementValue] {
        override def prettyPrec(p:Int, ev:ElementValue):Doc = ev match {
            case EVVal(vi) => viPty.prettyPrec(p,vi)
            case EVAnn(ann) => annPty.prettyPrec(p,ann)
        }
    }
    
    // ------------------------------------------------------------------------------
    // Statements

    implicit def blockPretty(implicit bstmtPty:Pretty[BlockStmt]) = new Pretty[Block] {
        override def prettyPrec(p:Int, blk:Block): Doc = blk match {
            case Block(stmts) => braceBlock(stmts.map(bstmtPty.prettyPrec(p,_)))
        }
    }

    implicit def blockStmtPretty(implicit stmtPty:Pretty[Stmt]
                                , cdPty:Pretty[ClassDecl]
                                , modPty:Pretty[Modifier]
                                , tyPty:Pretty[Type]
                                , vdPty:Pretty[VarDecl]) = new Pretty[BlockStmt] {
        override def prettyPrec(p:Int, blkStmt:BlockStmt): Doc = blkStmt match {
            case BlockStmt_(stmt) => stmtPty.prettyPrec(p,stmt)
            case LocalClass(cd) => cdPty.prettyPrec(p,cd)
            case LocalVars(mods,t,vds) => {
                val pmods = hsep(mods.map(modPty.prettyPrec(p,_)))
                val pty = tyPty.prettyPrec(p,t)
                val pvds = hsep(punctuate(comma, vds.map(vdPty.prettyPrec(p,_)))) + semi
                hsep(List(pmods, pty, pvds))
            }
        }
    }

    implicit def stmtPretty(implicit blkPty:Pretty[Block]
                            , expPty:Pretty[Exp]
                            , stmtPty:Pretty[Stmt]
                            , forInitPty:Pretty[ForInit]
                            , tyPty:Pretty[Type]
                            , idPty:Pretty[Ident]
                            , modPty:Pretty[Modifier]
                            , sBlkPty:Pretty[SwitchBlock] ) = new Pretty[Stmt] {
        override def prettyPrec(p:Int, stmt:Stmt) : Doc = stmt match {
            case StmtBlock(block) => blkPty.prettyPrec(p,block)
            case IfThen(c,th) => {
                val p1 = text("if")
                val p2 = parens(expPty.prettyPrec(p,c)) // TODO:double check in haskell version p was 0
                val p3 = prettyNestedStmt(0,th) // TODO double check
                stack(List(hsep(List(p1,p2)), p3))
            }
            case IfThenElse(c,th,el) => {
                val p1 = text("if")
                val p2 = parens(expPty.prettyPrec(p,c))
                val p3 = prettyNestedStmt(0,th) // TODO double check
                val p4 = prettyNestedStmt(0,el) // TODO double check
                stack(List(hsep(List(p1,p2)), p3, text("else"), p4))
            }
            case While(c,stmt) => {
                val p1 = text("while") 
                val p2 = parens(expPty.prettyPrec(p,c))
                val p3 = prettyNestedStmt(0,stmt)
                stack(List(hsep(List(p1,p2)),p3))
            }
            
            case BasicFor(mInit, mE, mUp, stmt) => {
                val p1 = text("for")
                def eachUp(up:List[Exp]):Doc = hsep(punctuate(comma,up.map(expPty.prettyPrec(p,_))))
                val p2 = parens(hsep(List(maybePP(p,mInit)
                                        , semi
                                        , maybePP(p,mE)
                                        , semi
                                        , maybe(empty, eachUp, mUp)
                                        )))
                val p3 = prettyNestedStmt(p,stmt) 
                stack(List(hsep(List(p1, p2)), p3))
            }
            case EnhancedFor(mods, t, ident, e, stmt) => {
                val p1 = text("for")
                val p2 = parens(hsep(List(hsep(mods.map(modPty.prettyPrec(p,_)))
                                        , tyPty.prettyPrec(p,t)
                                        , idPty.prettyPrec(p,ident)
                                        , colon
                                        , expPty.prettyPrec(p,e))))
                val p3 = stmtPty.prettyPrec(p,stmt)
                hsep(List(p1,p2,p3))
            }
            case Empty => semi
            case ExpStmt(e) => expPty.prettyPrec(p,e) + semi
            case Assert(ass,mE) => {
                val p1 = text("assert")
                val p2 = expPty.prettyPrec(p,ass)
                def f(e:Exp):Doc = colon + expPty.prettyPrec(p,e)
                val p3 = maybe(empty, f, mE) + semi
                hsep(List(p1,p2,p3))
            }

            case Switch(e,sBlocks) => {
                val p1 = text("switch") 
                val p2 = parens(expPty.prettyPrec(p,e))
                val p3 = braceBlock(sBlocks.map(sBlkPty.prettyPrec(p,_)))
                stack(List(hsep(List(p1,p2)),p3))
            }
        }
    }

    def ppImplements(prec:Int,impls:List[RefType]):Doc = empty
    def ppTypeParams[A](prec:Int,typeParams:List[A])(implicit ppa:Pretty[A]):Doc = empty
    def ppExtends(prec:Int,exts:List[RefType]):Doc = empty
    def ppArgs[A](prec:Int, args:List[A])(implicit ppa:Pretty[A]):Doc = empty
    def ppResultType(prec:Int, mt:Option[Type]):Doc = empty
    def ppThrows(prec:Int, throws:List[ExceptionType]):Doc = empty
    def ppDefault(prec:Int, deft:Option[Exp]):Doc = empty


    implicit val interfaceDeclPretty = new Pretty[InterfaceDecl] {
        override def prettyPrec(p:Int, idecl:InterfaceDecl) = empty
    }

    implicit val namePretty = new Pretty[Name] {
        override def prettyPrec(p:Int, name:Name) = empty
    }

    
    val semi:Doc = char(';')

    // ---------------------------------------------------------------------
    // Help functionality 

    def vcat(ds:List[Doc]):Doc = stack(ds)

    val colon:Doc = char(':')

    def opt(x:Boolean, a:Doc):Doc = if (x) a else empty

    def parens(p:Doc):Doc = char('(') + p + char(')')
    def braceBlock(xs:List[Doc]):Doc = { // TODO: shall we use bracketBy
        stack(List(char('{'), nest(2,vcat(xs)), char('}')))
    }

    def nest(k:Int, p:Doc):Doc = p.indent(k)

    def prettyNestedStmt(prio:Int, p:Stmt)(implicit stmtPty:Pretty[Stmt]): Doc = p match {
        case StmtBlock(b) => stmtPty.prettyPrec(prio,p) 
        case _ => nest(2, stmtPty.prettyPrec(prio,p))
    }

    def maybePP[A](p:Int, mba:Option[A])(implicit ppa:Pretty[A]):Doc = mba match {
        case None => empty
        case Some(a) => ppa.prettyPrec(p,a)
    }
    def maybe[A,B](default:B,f:(A => B), mba:Option[A]):B = mba match {
        case None => default
        case Some(a) => f(a)
    }

    def hsep(ds:List[Doc]):Doc = spread(ds.filter(!_.isEmpty))

    def punctuate(p:Doc, ds:List[Doc]):List[Doc] = ds match {
        case Nil => Nil
        case (x::xs) => { 
            def go(y:Doc,zs:List[Doc]):List[Doc] = zs match {
                case Nil => List(y)
                case (z::zss) => (y+p) :: (go(z,zss))
            }
            go(x,xs)
        }
    }

}