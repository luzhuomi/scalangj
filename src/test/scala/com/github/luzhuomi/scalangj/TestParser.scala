package com.github.luzhuomi.scalangj

import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Parser._ // To unify the base class JavaToken, otherwise === will failed
import com.github.luzhuomi.scalangj.Syntax._
import org.scalatest.{FunSuite, Matchers}



class TestLitParser1 extends FunSuite with Matchers {
  val STRING = "1"
  val LITERAL = IntLit(1)
  test("Literal is parsed correlectly") {
    val result = parseLiteral(STRING)
    assert((result.successful) && (result.get === LITERAL))
  }
}


class TestParser1 extends FunSuite with Matchers {
  val STRING = "int x = 1;"
  val ty:Type = PrimType_(IntT)
  val vardecls:List[VarDecl] = List(VarDecl(VarId(Ident("x")), Some(InitExp(Lit(IntLit(1))))))
  val LOCALVARDECL:(List[Modifier], Type, List[VarDecl]) = (Nil, ty, vardecls)
  test(s"phrase ${STRING} is parsed correctly") {
    val result = localVarDecl(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === LOCALVARDECL))
  }
}


class TestParser2 extends FunSuite with Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
        System.out.println("Hello World!");
    }
} 
  """
  val CLASSDECL = ClassTypeDecl(ClassDecl_(List(Public),Ident("HelloWorld"),List(),None,List()
      ,ClassBody(List(MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,MethodBody(Some(Block(List(BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))))))))))))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === CLASSDECL))
  }
}


class TestParser3 extends FunSuite with Matchers {
  val STRING = """
        System.out.println("Hello World!");
  """
  val METHODINV = MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = methodInvocationExp.apply(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === METHODINV))
  }
}


class TestParser4 extends FunSuite with Matchers {
  val STRING = "System.out.println"
  val NAME = Name(List(Ident("System"), Ident("out"), Ident("println")))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = name.apply(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === NAME))
  }
}

class TestParser5 extends FunSuite with Matchers {
  val STRING = """
  public class Fib
{
    public static int fib(int n)
    {
	int f1 = 0;
	int f2 = 1;
	int i=0;
	while(i<n) {
	    int t = f2;
	    f2 = f1 + f2;
	    f1 = t;
	    i++;
	}
	return f2;
    }

    public static void  main(String argv[]) {
	System.out.println(fib(10));
    }
}

  """
  val CLASSDECL:TypeDecl = ClassTypeDecl(
    ClassDecl_(
      List(Public),Ident("Fib"),List(),None,List(),
      ClassBody(List(
      MemberDecl_(MethodDecl(List(Public, Static),List(),
        Some(PrimType_(IntT)),Ident("fib"),
          List(FormalParam(List(),PrimType_(IntT),false,VarId(Ident("n")))),
          List(),None,
          MethodBody(Some(Block(List(LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("f1")),
                                              Some(InitExp(Lit(IntLit(0))))))), 
                                  LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("f2")),
                                              Some(InitExp(Lit(IntLit(1))))))), 
                                  LocalVars(List(),PrimType_(IntT),
                                            List(VarDecl(VarId(Ident("i")),
                                              Some(InitExp(Lit(IntLit(0))))))), 
                                  BlockStmt_(While(BinOp(ExpName(Name(List(Ident("i")))),
                                                      LThan,ExpName(Name(List(Ident("n"))))),
                                                  StmtBlock(Block(List(
                                                          LocalVars(List(),
                                                              PrimType_(IntT),
                                                              List(VarDecl(VarId(Ident("t")),
                                                                  Some(InitExp(ExpName(Name(List(Ident("f2"))))))))), 
                                                          BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("f2")))),
                                                                                  EqualA,
                                                                                  BinOp(ExpName(Name(List(Ident("f1")))),Add,ExpName(Name(List(Ident("f2")))))))), 
                                                          BlockStmt_(ExpStmt(Assign(NameLhs(Name(List(Ident("f1")))),
                                                                                  EqualA,
                                                                                  ExpName(Name(List(Ident("t"))))))), 
                                                          BlockStmt_(ExpStmt(PostIncrement(ExpName(Name(List(Ident("i")))))))))))), 
                                  BlockStmt_(Return(Some(ExpName(Name(List(Ident("f2"))))))))))))), 
        MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),
          List(FormalParam(List(),RefType_(ClassRefType(ClassType(List((Ident("String"),List()))))),
            false,VarDeclArray(VarId(Ident("argv"))))),List(),None,
            MethodBody(Some(Block(List(
                BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println")))
                  ,List(MethodInv(MethodCall(Name(List(Ident("fib"))),List(Lit(IntLit(10))))))))))))))))))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === CLASSDECL))
  }
}




class TestParser6 extends FunSuite with Matchers {
  val STRING = """
public class HelloWorld {
    public static void main() {
      int x = 0;
      try
      { x = x / 0; }
      catch (Exception exception_desugured)
      {
        
      }
      finally{ }
    }
} 
  """
  val CLASSDECL = ClassTypeDecl(ClassDecl_(List(Public),Ident("HelloWorld"),List(),None,List()
      ,ClassBody(List(MemberDecl_(MethodDecl(List(Public, Static),List(),None,Ident("main"),List(),List(),None,MethodBody(Some(Block(List(BlockStmt_(ExpStmt(MethodInv(MethodCall(Name(List(Ident("System"), Ident("out"), Ident("println"))),List(Lit(StringLit("Hello World!")))))))))))))))))
  test(s"phrase ${STRING} is parsed correctly") {
    val result = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING))
    assert((result.successful) && (result.get === CLASSDECL))
  }
}