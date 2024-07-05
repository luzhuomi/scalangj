# Project description 

* This is a Scala port of language-java from Haskell `https://hackage.haskell.org/package/language-java`
* Consists of a Lexer, a Parser and a Pretty Printer, written in Scala


# Setup SBT dependency
If you are using SBT to manage your project, to add this library as a dependency in  your  project, include the following in the  `build.sbt` file

```sbt
resolvers += "obsidian binary github repo" at "https://raw.githubusercontent.com/obsidian-java/binrepo/master/"

libraryDependencies += "obsidian.lang.java" %% "scalangj" % "0.1.5" 
```

# Example 

```scala
import obsidian.lang.java.scalangj.Parser.*
import obsidian.lang.java.scalangj.Pretty.*


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



