package com.github.luzhuomi.scalangj


import com.github.luzhuomi.scalangj._
import com.github.luzhuomi.scalangj.Parser._ // To unify the base class JavaToken, otherwise === will failed
import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Pretty._
import com.github.luzhuomi.scalangj.Pretty.ops._
import org.scalatest.{FunSuite, Matchers}



class TestPretty1 extends FunSuite with Matchers {
    val STRING = """
public class HelloWorld {
    public static void main() {
        char c = 'a';
        System.out.println("Hello World!");
    }
} 
"""
    val tyDec:TypeDecl = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING)).get

    test(s"phrase ${STRING} is parsed correctly") {
        val o = prettyPrint(tyDec) 
        println(o)
        assert(o != null)
    }
}

class TestPretty2 extends FunSuite with Matchers {
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
    val tyDec:TypeDecl = classOrInterfaceDecl.apply(new Lexer.Scanner(STRING)).get

    test(s"phrase ${STRING} is parsed correctly") {
        val o = prettyPrint(tyDec) 
        println(o)
        assert(o != null)
    }
}

