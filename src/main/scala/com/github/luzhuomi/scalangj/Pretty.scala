package com.github.luzhuomi.scalangj

import org.typelevel.paiges._
import org.typelevel.paiges.Doc._
import com.github.luzhuomi.scalangj.Syntax._

object Pretty {

    def prettyPrint[A](a:A)(implicit pa:Pretty[A]) : String = {
        pa.pretty(a).toString()
    }

    def parenPrec(inheritedPrec:Int, currentPrec:Int, t:Doc):Doc = {
        if (inheritedPrec <= 0) {
            t
        } else if (inheritedPrec < currentPrec) {
            parens(t)
        }
        else {
            t
        }
    }

    trait Pretty[A] {
        def pretty(a:A): Doc = prettyPrec(0,a)
        def prettyPrec(prec:Int, a:A):Doc = pretty(a)
 
    }
    
    // ----------------------------------------------------------------------------
    // Ops code
    // will consolidate the implicit resolution here first then outside
    // if the resolution happens at each individual instance (i.e. implicit def or val)
    // the prettyPrec has to annotated with some extra implicit para, which leads into divergence
    // in the resolution process
    object ops {
        def prettyPrec[A](p:Int,a:A)(implicit aPretty:Pretty[A]):Doc = {
            aPretty.prettyPrec(p,a)
        }
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
                                , tpPty:Pretty[TypeParam]
                                , rtPty:Pretty[RefType]) = new Pretty[ClassDecl] {
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
                                    , tpPty:Pretty[TypeParam]
                                    , rtPty:Pretty[RefType]) = new Pretty[InterfaceDecl] 
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
                                , tpPty:Pretty[TypeParam]
                                , etPty:Pretty[ExceptionType]
                                , expPty:Pretty[Exp]) = new Pretty[MemberDecl] {
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
                            , sBlkPty:Pretty[SwitchBlock]
                            , catPty:Pretty[Catch] ) = new Pretty[Stmt] {
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

            case Do(stmt,e) => {
                val p1 = text("do")
                val p2 = stmtPty.prettyPrec(p,stmt)
                val p3 = text("while")
                val p4 = parens(expPty.prettyPrec(p,e)) + semi
                stack(List(p1, hsep(List(p2,p3,p4))))
            }

            case Break(mIdent) => {
                val p1 = text("break") 
                val p2 = maybePP(p,mIdent) + semi
                hsep(List(p1,p2))
            }

            case Continue(mIdent) => {
                val p1 = text("continue") 
                val p2 = maybePP(p,mIdent) + semi
                hsep(List(p1,p2))
            }

            case Return(mE) => {
                val p1 = text("return")
                val p2 = maybePP(p,mE) + semi
                hsep(List(p1,p2))
            }

            case Synchronized(e,block) => {
                val p1 = text("synchronized") 
                val p2 = parens(expPty.prettyPrec(p,e))
                val p3 = blkPty.prettyPrec(p,block) 
                stack(List(hsep(List(p1,p2)), p3))
            }

            case Throw(e) => {
                val p1 = text("throw")
                val p2 = expPty.prettyPrec(p,e) + semi
                hsep(List(p1,p2))
            }

            case Try(block, catches, mFinally) => {
                val p1 = text("try")
                val p2 = blkPty.prettyPrec(p,block)
                def ppFinally(mBlk:Option[Block]) :Doc = mBlk match {
                    case None => empty
                    case Some(bl) => hsep(List(text("finally"), blkPty.prettyPrec(p,bl)))
                }
                val p3 = vcat(catches.map(catPty.prettyPrec(p,_)) ++ List(ppFinally(mFinally)))
                stack(List(p1,p2,p3))
            }
            
            case Labeled(ident,stmt) => {
                val p1 = idPty.prettyPrec(p,ident) + colon
                val p2 = stmtPty.prettyPrec(p,stmt) 
                hsep(List(p1,p2))
            }
        }
    }

    implicit def catchPretty(implicit blkPty:Pretty[Block]
                            , fpPty:Pretty[FormalParam]) = new Pretty[Catch] {
        override def prettyPrec(p:Int, cat:Catch):Doc = cat match {
            case Catch(fParam,block) => {
                val p1 = hsep(List(text("catch"), parens(fpPty.prettyPrec(p,fParam))))
                val p2 = blkPty.prettyPrec(p,block) 
                stack(List(p1,p2))
            }
        }
    }

    implicit def switchBlockPretty(implicit lblPty:Pretty[SwitchLabel]
                                , bstmtPty:Pretty[BlockStmt]) = new Pretty[SwitchBlock] {
        override def prettyPrec(p:Int, sb:SwitchBlock) = sb match {
            case SwitchBlock(lbl,stmts) => {
                vcat( lblPty.prettyPrec(p,lbl) :: stmts.map(s => nest(2,bstmtPty.prettyPrec(p,s))))
            }
        }
    }

    implicit def switchLabelPretty(implicit expPty:Pretty[Exp]) = new Pretty[SwitchLabel] {
        override def prettyPrec(p:Int, sl:SwitchLabel) = sl match  {
            case SwitchCase(e) => {
                hsep(List(text("case"), expPty.prettyPrec(p,e)+colon))
            }
            case Default => text("default:")
        }
    }

    implicit def forInitPretty(implicit tyPty:Pretty[Type]
                            , vdPty:Pretty[VarDecl]
                            , modPty:Pretty[Modifier]
                            , expPty:Pretty[Exp]) = new Pretty[ForInit] {
        override def prettyPrec(p:Int, fi:ForInit) = fi match {
            case ForLocalVars(mods,t,vds) => {
                hsep( mods.map(modPty.prettyPrec(p,_))
                    ++ (tyPty.prettyPrec(p,t)::punctuate(comma,vds.map(vdPty.prettyPrec(p,_)))))
            }
            case ForInitExps(es) => {
                hsep(punctuate(comma, es.map(expPty.prettyPrec(p,_))))
            }
        }
    }

    // -----------------------------------------------------------------------------
    // Expressions

    implicit def expPretty(implicit namePty:Pretty[Name]
                        , litPty:Pretty[Literal]
                        , tdsPty:Pretty[TypeDeclSpecifier]
                        , tarPty:Pretty[TypeArgument]
                        , arPty:Pretty[Argument]
                        , cbodyPty:Pretty[ClassBody]
                        , idPty:Pretty[Ident]
                        , tyPty:Pretty[Type]
                        , miPty:Pretty[MethodInvocation]
                        , aiPty:Pretty[ArrayInit]
                        , aidxPty:Pretty[ArrayIndex]
                        , faPty:Pretty[FieldAccess]
                        , opPty:Pretty[Op]
                        , rtPty:Pretty[RefType]
                        , lhsPty:Pretty[Lhs]
                        , aoPty:Pretty[AssignOp]
                        , lampPty:Pretty[LambdaParams]
                        , lamePty:Pretty[LambdaExpression]) = new Pretty[Exp] {
        override def prettyPrec(p:Int, exp:Exp):Doc = exp match {
            case Lit(l) => litPty.prettyPrec(p,l)
            case ClassLit(mT) => {
                ppResultType(p,mT) + text(".class")
            }
            case This => text("this")
            case ThisClass(name) => namePty.prettyPrec(p,name) + text(".this")
            case InstanceCreation(tArgs,tds,args,mBody) => {
                val p1 = hsep(List(text("new")
                        , ppTypeParams(p,tArgs)
                        , tdsPty.prettyPrec(p,tds)+ppArgs(p,args)(arPty)))
                val p2 = maybePP(p,mBody)
                stack(List(p1,p2))
            }
            case QualInstanceCreation(e,tArgs,ident,args,mBody) => {
                val p1 = hsep(List((prettyPrec(p,e) + char(',') + text("new"))
                        , ppTypeParams(p,tArgs)
                        , idPty.prettyPrec(p,ident) + ppArgs(p,args)(arPty)))
                val p2 = maybePP(p,mBody)
                stack(List(p1,p2))
            }
            case ArrayCreate(t,es,k) => {
                val p1 = text("new")
                val p2 = hcat(tyPty.prettyPrec(p,t)::(es.map(e => brackets(prettyPrec(p,e))) ++ List.fill(k)(text("[]"))))
                hsep(List(p1,p2))
            }
            case ArrayCreateInit(t,k,init) => {
                val p1 = text("new")
                val p2 = hcat(tyPty.prettyPrec(p,t)::List.fill(k)(text("[]")))
                val p3 = aiPty.prettyPrec(p,init)
                hsep(List(p1,p2,p3))
            }
            case FieldAccess_(fa) => {
                parenPrec(p,1,faPty.prettyPrec(1,fa)) // TODO: check
            }
            case MethodInv(mi) => parenPrec(p,1, miPty.prettyPrec(1,mi))
            case ArrayAccess(ain) => parenPrec(p,1,aidxPty.prettyPrec(1,ain))
            case ExpName(name) => namePty.prettyPrec(p,name)
            case PostIncrement(e) => parenPrec(p,1,prettyPrec(2,e)+text("++"))
            case PostDecrement(e) => parenPrec(p,1,prettyPrec(2,e)+text("--"))
            case PreIncrement(e) => parenPrec(p,1,text("++")+prettyPrec(2,e))
            case PreDecrement(e) => parenPrec(p,1,text("--")+prettyPrec(2,e))
            case PrePlus(e) => parenPrec(p,2,char('+')+prettyPrec(2,e))
            case PreMinus(e) => parenPrec(p,2,char('-')+prettyPrec(2,e))
            case PreBitCompl(e) => parenPrec(p,2,char('~')+prettyPrec(2,e))
            case PreNot(e) => parenPrec(p,2,char('!')+prettyPrec(2,e))
            case Cast(t,e) => parenPrec(p,2, hsep(List(parens(tyPty.prettyPrec(p,t)), prettyPrec(2,e))))
            case BinOp(e1,op,e2) => {
                val prec = opPrec(op) 
                parenPrec(p,prec, hsep(List(prettyPrec(prec,e1), opPty.prettyPrec(p,op), prettyPrec(prec,e2))))
            }
            case InstanceOf(e,rt) => {
                val cp = opPrec(LThan)
                parenPrec(p,cp, hsep(List(prettyPrec(cp,e), text("instanceof"), rtPty.prettyPrec(cp,rt))))
            }
            case Cond(c,th,el) => {
                parenPrec(p,13, hsep(List(prettyPrec(13,c), char('?'), prettyPrec(p,th), colon, prettyPrec(13,el)))) // TODO: checl
            }
            case Assign(lhs,aop,e) => {
                hsep(List(lhsPty.prettyPrec(p,lhs), aoPty.prettyPrec(p,aop), prettyPrec(p,e)))
            }
            case Lambda(params,body) => {
                hsep(List(lampPty.prettyPrec(p,params), text("->"), lamePty.prettyPrec(p,body)))
            }
            case MethodRef(i1,i2) => {
                hsep(List(namePty.prettyPrec(p,i1), text("::"), idPty.prettyPrec(p,i2)))
            }
        }
    }
    
    implicit def lambdaParamsPretty(implicit idPty:Pretty[Ident]
                                    , pfPty:Pretty[FormalParam]) = new Pretty[LambdaParams] {
        override def prettyPrec(p:Int, lps:LambdaParams) : Doc = lps match {
            case LambdaSingleParam(ident) => idPty.prettyPrec(p,ident)
            case LambdaFormalParams(params) => ppArgs(p,params)
            case LambdaInferredParams(idents) => ppArgs(p,idents)
        }
    }

    implicit def LambdaExpressionPretty(implicit blkPty:Pretty[Block]
                                        , expPty:Pretty[Exp]) = new Pretty[LambdaExpression] {
        override def prettyPrec(p:Int, le:LambdaExpression):Doc = le match {
            case LambdaExpression_(exp) => expPty.prettyPrec(p,exp)
            case LambdaBlock(block) => blkPty.prettyPrec(p,block)
        }
    }

    implicit val literalPretty = new Pretty[Literal] {
        override def prettyPrec(p:Int, lit:Literal) : Doc = lit match {
            case IntLit(i) => text(i.toString())
            case LongLit(i) => text(i.toString()) + char('L')
            case FloatLit(f) => text(f.toString()) + char('F') 
            case DoubleLit(d) => text(d.toString())
            case BooleanLit(b) => text(b.toString().toLowerCase())
            case CharLit(c) => quotes(text(escapeChar(c)))
            case StringLit(s) => doubleQuotes (text(s.flatMap(escapeChar(_))))
            case NullLit => text("null")
        }
    }

    implicit val opPretty = new Pretty[Op] {
        override def prettyPrec(p:Int, op:Op) : Doc = {
            val s = op match {
                case Mult    => "*"
                case Div     => "/"
                case Rem     => "%"
                case Add     => "+"
                case Sub     => "-"
                case LShift  => "<<"
                case RShift  => ">>"
                case RRShift => ">>>"
                case LThan   => "<"
                case GThan   => ">"
                case LThanE  => "<="
                case GThanE  => ">="
                case Equal   => "=="
                case NotEq   => "!="
                case And     => "&"
                case Xor     => "^"
                case Or      => "|"
                case CAnd    => "&&"
                case COr     => "||"
            }
            text(s) 
        }
    }

    implicit val assignOpPretty = new Pretty[AssignOp] {
        override def prettyPrec(p:Int, aop:AssignOp) : Doc = {
            val s = aop match {
                case EqualA   => "="
                case MultA    => "*="
                case DivA     => "/="
                case RemA     => "%="
                case AddA     => "+="
                case SubA     => "-="
                case LShiftA  => "<<="
                case RShiftA  => ">>="
                case RRShiftA => ">>>="
                case AndA     => "&="
                case XorA     => "^="
                case OrA      => "|="
            }
            text(s)
        }
    }

    implicit def lhsPretty(implicit namePty:Pretty[Name]
                        , faPty:Pretty[FieldAccess]
                        , ainPty:Pretty[ArrayIndex]) = new Pretty[Lhs] {
        override def prettyPrec(p:Int, lhs:Lhs):Doc = lhs match {
            case NameLhs(name) => namePty.prettyPrec(p,name)
            case FieldLhs(fa)  => faPty.prettyPrec(p,fa)
            case ArrayLhs(ain) => ainPty.prettyPrec(p,ain)
        }
    }

    implicit def arrayIndexPretty(implicit expPty:Pretty[Exp]) = new Pretty[ArrayIndex] {
        override def prettyPrec(p:Int, ain:ArrayIndex):Doc = ain match {
            case ArrayIndex(ref,e) => expPty.prettyPrec(p,ref) + (hcat(e.map(x => brackets(expPty.prettyPrec(p,x)))))
        }
    }

    implicit def fieldAccessPretty(implicit idPty:Pretty[Ident]
                                , expPty:Pretty[Exp]
                                , namePty:Pretty[Name]) = new Pretty[FieldAccess] {
        override def prettyPrec(p:Int, fa:FieldAccess):Doc = fa match {
            case PrimaryFieldAccess(e, ident) => expPty.prettyPrec(p,e) + char('.') + idPty.prettyPrec(p,ident)
            case SuperFieldAccess(ident) => text("super.") + idPty.prettyPrec(p,ident)
            case ClassFieldAccess(name,ident) => namePty.prettyPrec(p,name) + char('.') + idPty.prettyPrec(p,ident)
        }
    }

    implicit def methodInvocationPretty(implicit expPty:Pretty[Exp]
                                        , namePty:Pretty[Name]
                                        , argPty:Pretty[Argument]
                                        , idPty:Pretty[Ident]
                                        , rtPty:Pretty[RefType]) = new Pretty[MethodInvocation] {
        override def prettyPrec(p:Int, mi:MethodInvocation):Doc = mi match {
            case MethodCall(name,args) => namePty.prettyPrec(p,name) + ppArgs(p,args)(argPty)
            case PrimaryMethodCall(e,tArgs,ident,args) => {
                val p1 = expPty.prettyPrec(p,e)
                val p2 = char('.')
                val p3 = ppTypeParams(p,tArgs)
                val p4 = idPty.prettyPrec(p,ident)
                val p5 = ppArgs(p,args)(argPty)
                hcat(List(p1,p2,p3,p4,p5))
            }
            case SuperMethodCall(tArgs,ident,args) => {
                val p1 = text("super.")
                val p2 = ppTypeParams(p,tArgs)
                val p3 = idPty.prettyPrec(p,ident)
                val p4 = ppArgs(p,args)(argPty)
                hcat(List(p1,p2,p3,p4))
            }
            case ClassMethodCall(name,tArgs,ident,args) => {
                val p1 = namePty.prettyPrec(p,name)
                val p2 = text(".super.")
                val p3 = ppTypeParams(p,tArgs)
                val p4 = idPty.prettyPrec(p,ident)
                val p5 = ppArgs(p,args)(argPty)
                hcat(List(p1,p2,p3,p4,p5))
            }
            case TypeMethodCall(name,tArgs,ident,args) => {
                val p1 = namePty.prettyPrec(p,name)
                val p2 = char('.')
                val p3 = ppTypeParams(p,tArgs)
                val p4 = idPty.prettyPrec(p,ident)
                val p5 = ppArgs(p,args)(argPty)
                hcat(List(p1,p2,p3,p4,p5))            
            }
        }
    }

    implicit def arrayInitPretty(implicit viPty:Pretty[VarInit])  = new Pretty[ArrayInit] {
        override def prettyPrec(p:Int, ai:ArrayInit):Doc = ai match {
            case ArrayInit(vInits) => braceBlock(vInits.map(v => viPty.prettyPrec(p,v) + comma))
        }
    }

    def ppArgs[A](p:Int, args:List[A])(implicit ppa:Pretty[A]):Doc = {
        parens(hsep(punctuate(comma, args.map(ppa.prettyPrec(p,_)))))
    }

    // ---------------------------------------------------------------------------
    // Types

    implicit def typePretty(implicit ptPty:Pretty[PrimType]
                            , rtPty:Pretty[RefType]) = new Pretty[Type] {
        override def prettyPrec(p:Int,t:Type):Doc = t match {
            case PrimType_(pt) => ptPty.prettyPrec(p,pt)
            case RefType_(rt) => rtPty.prettyPrec(p,rt) 
        }
    }

    implicit def refTypePretty(implicit ctPty:Pretty[ClassType]
                        , tyPty:Pretty[Type]) = new Pretty[RefType] {
        override def prettyPrec(p:Int, rt:RefType):Doc = rt match {
            case ClassRefType(ct) => ctPty.prettyPrec(p,ct) 
            case ArrayType(t) => tyPty.prettyPrec(p,t) + text("[]")
        }
    }

    implicit def classTypePretty(implicit idPty:Pretty[Ident]
                                , targPty:Pretty[TypeArgument]) = new Pretty[ClassType] {
        override def prettyPrec(p:Int, ct:ClassType):Doc = ct match {
            case ClassType(itas) => {
                hcat(punctuate(char('.'), itas.map( x => x match { 
                    case (i,tas) => idPty.prettyPrec(p,i) + ppTypeParams(p,tas)
                })))
            }
        }
    }

    implicit def typeArgumentPretty(implicit rtPty:Pretty[RefType]
                                  , wbPty:Pretty[WildcardBound]) = new Pretty[TypeArgument] {
        override def prettyPrec(p:Int, targ:TypeArgument):Doc = targ match {
            case ActualType(rt) => rtPty.prettyPrec(p,rt)
            case Wildcard(mBound) => hsep(List(char('?'), maybePP(p,mBound)))
        }
    }

    implicit def typeDeclSpecifierPretty(implicit ctPty:Pretty[ClassType]
                                        , idPty:Pretty[Ident]
                                        , dmPty:Pretty[Diamond]) = new Pretty[TypeDeclSpecifier] {
        override def prettyPrec(p:Int, tds:TypeDeclSpecifier):Doc = tds match {
            case TypeDeclSpecifier_(ct) => ctPty.prettyPrec(p,ct)
            case TypeDeclSpecifierWithDiamond(ct, i, d) => ctPty.prettyPrec(p,ct) + char('.') + idPty.prettyPrec(p,i) + dmPty.prettyPrec(p,d) 
            case TypeDeclSpecifierUnqualifiedWithDiamond(i, d) => idPty.prettyPrec(p,i) + dmPty.prettyPrec(p,d)
        }
    }

    implicit val diamondPretty = new Pretty[Diamond] {
        override def prettyPrec(p:Int, d:Diamond):Doc = text("<>")
    }

    implicit def wildcardBoundPretty(implicit rtPty:Pretty[RefType]) = new Pretty[WildcardBound] {
        override def prettyPrec(p:Int, b:WildcardBound):Doc = b match {
            case ExtendsBound(rt) => hsep(List(text("extends"), rtPty.prettyPrec(p,rt)))
            case SuperBound(rt)   => hsep(List(text("super"), rtPty.prettyPrec(p,rt)))
        }
    }

    implicit val primTypePretty  = new Pretty[PrimType] { 
        override def prettyPrec(p:Int, pt:PrimType):Doc = pt match {
            case BooleanT => text("boolean")
            case ByteT    => text("byte")
            case ShortT   => text("short")
            case IntT     => text("int")
            case LongT    => text("long")
            case CharT    => text("char")
            case FloatT   => text("float")
            case DoubleT  => text("double")
        }
    }

    implicit def typeParamPretty(implicit idPty:Pretty[Ident]
                                , rtPty:Pretty[RefType]) = new Pretty[TypeParam] {
        override def prettyPrec(p:Int, tp:TypeParam):Doc = tp match {
            case TypeParam(ident,rts) => {
                val p1 = idPty.prettyPrec(p,ident)
                val p2 = opt(rts.length > 0, hsep(text("extends")::punctuate(text(" &"), rts.map(rtPty.prettyPrec(p,_)))))
                hsep(List(p1,p2))
            }  
        }
    }

    def ppTypeParams[A](p:Int,typeParams:List[A])(implicit ppa:Pretty[A]):Doc = typeParams match {
        case Nil => empty
        case tps => {
            char('<') + hsep(punctuate(comma,tps.map(ppa.prettyPrec(p,_)))) + char('>')
        }
    }

    def ppImplements(p:Int,impls:List[RefType])(implicit rtPty:Pretty[RefType]):Doc = impls match {
        case Nil => empty
        case rts => hsep(List(text("implements"), hsep(punctuate(comma, rts.map(rtPty.prettyPrec(p,_))))))
    }

    def ppExtends(p:Int,exts:List[RefType])(implicit rtPty:Pretty[RefType]):Doc = exts match {
        case Nil => empty
        case rts => hsep(List(text("extends"), hsep(punctuate(comma, rts.map(rtPty.prettyPrec(p,_))))))
    }
    def ppThrows(p:Int, throws:List[ExceptionType])(implicit etPty:Pretty[ExceptionType]):Doc = throws match {
        case Nil => empty
        case ets => hsep(List(text("throws"), hsep(punctuate(comma, ets.map(etPty.prettyPrec(p,_))))))
    }
    def ppDefault(p:Int, deft:Option[Exp])(implicit expPty:Pretty[Exp]):Doc = deft match {
        case None      => empty
        case Some(exp) => hsep(List(text("default"), expPty.prettyPrec(p,exp)))
    } 
    def ppResultType(p:Int, mt:Option[Type])(implicit tyPty:Pretty[Type]):Doc = mt match {
        case None    => text("void")
        case Some(a) =>  tyPty.prettyPrec(p,a) 
    }

    
    // --------------------------------------------------------------------
    // Names and identifiers

    implicit def namePretty(implicit idPty:Pretty[Ident]) = new Pretty[Name] {
        override def prettyPrec(p:Int, name:Name):Doc = name match {
            case Name(is) => {
                hcat(punctuate(char('.'), is.map(idPty.prettyPrec(p,_))))
            }
        }
    }

    implicit val identPretty = new Pretty[Ident] {
        override def prettyPrec(p:Int, id:Ident):Doc = id match {
            case Ident(s) => text(s)
        }
    }
    
    val semi:Doc = char(';')

    // ---------------------------------------------------------------------
    // Help functionality 

    def vcat(ds:List[Doc]):Doc = stack(ds)
    def hcat(ds:List[Doc]):Doc = fill(empty, ds)

    val colon:Doc = char(':')

    def opt(x:Boolean, a:Doc):Doc = if (x) a else empty

    def parens(p:Doc):Doc = char('(') + p + char(')')
    def brackets(p:Doc):Doc = char('[') + p + char(']')
    def quotes(p:Doc):Doc = char('\'') + p + char('\'')
    def doubleQuotes(p:Doc):Doc = char('"') + p + char('"')
    def braceBlock(xs:List[Doc]):Doc = { // TODO: shall we use bracketBy
        // stack(List(char('{'), nest(2,vcat(xs)), char('}')))
        vcat(xs).bracketBy(char('{'), char('}'),2)
    }

    def nest(k:Int, p:Doc):Doc = p.indent(k)


    def opPrec(op:Op):Int = op match {
        case Mult    => 3
        case Div     => 3
        case Rem     => 3
        case Add     => 4
        case Sub     => 4
        case LShift  => 5
        case RShift  => 5
        case RRShift => 5
        case LThan   => 6
        case GThan   => 6
        case LThanE  => 6
        case GThanE  => 6
        case Equal   => 7
        case NotEq   => 7
        case And     => 8
        case Xor     => 9
        case Or      => 10
        case CAnd    => 11
        case COr     => 12
    }

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

    def escapeGeneral(c:Char):String = c match {
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '\\' => "\\\\"
        case _ if c >= ' ' && c < 127 => c.toString()
        case _ if c <= 65535 => s"\\u${c.toInt}"
        case _ => sys.error(s"scalangj.Pretty.escapeGeneral: Char ${c} too large for Java Char")
    }

    def escapeChar(c:Char):String = c match {
        case '\'' => "\\'"
        case _    => escapeGeneral(c)
    }

    def escapeString(c:Char):String = c match {
        case '"' => """\\\"""""
        case _ if c <= 65535 => escapeGeneral(c)
        case _  => {
            val c_ = c.toInt - 0x010000
            val lead = 0xD8000 + c_ / 0x0400
            val trail = 0xDC00 + c_ % 0x0400
            escapeGeneral(lead.toChar) ++ escapeGeneral(trail.toChar)
        }
    }
}