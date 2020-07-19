package com.github.luzhuomi.scalangj

import org.typelevel.paiges._
import org.typelevel.paiges.Doc._
import com.github.luzhuomi.scalangj.Syntax._
import cats.implicits._
import cats._



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
                spread(List(text("package"), namePty.prettyPrec(p,name), semi)) 
        }
    }

    implicit def importDeclPretty(implicit namePty:Pretty[Name]) = new Pretty[ImportDecl]  {
        override def prettyPrec(p:Int, idecl:ImportDecl) = idecl match {
            case ImportDecl(st,name,wc) =>
                spread(List(text("import"), opt(st,text("static")), namePty.prettyPrec(p,name), opt(wc, text(".*")), semi))
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

    implicit val classDeclPretty = new Pretty[ClassDecl] {
        override def prettyPrec(p:Int, cdecl:ClassDecl) = cdecl match {
            case EnumDecl(mods,ident,impls,body) => empty 
            case ClassDecl_(mods,ident,tPArams,mSuper,impls,body) => empty
        }
    }

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

    def opt(x:Boolean, a:Doc):Doc = if (x) a else empty

    def maybePP[A](p:Int, mba:Option[A])(implicit ppa:Pretty[A]):Doc = mba match {
        case None => empty
        case Some(a) => ppa.prettyPrec(p,a)
    }


}