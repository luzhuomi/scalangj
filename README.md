# Project description 

* This is a Scala port of language-java from Haskell `https://hackage.haskell.org/package/language-java`
* Consists of a Lexer, a Parser and a Pretty Printer, written in Scala


# Setup SBT dependency
If you are using SBT to manage your project, to add this library as a dependency in  your  project, include the following in the  `build.sbt` file

```sbt
resolvers += "luzhuomi github repo" at "https://raw.githubusercontent.com/luzhuomi/mavenrepo/master/"

libraryDependencies += "com.github.luzhuomi" %% "scalangj" % "0.1.1"  
```

# Example 

```scala
import com.github.luzhuomi.scalangj.Parser._
import com.github.luzhuomi.scalangj.Pretty._


object Main extends App {
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
    val eCU = parseCompilationUnit(STRING)
    eCU match {
        case Left(error_msg) => println(error_msg)
        case Right(cu) => println(prettyPrint(cu)) 
    }
}
```



