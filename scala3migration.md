# Migration From Scala 2.12 to Scala 3
First, the scalaVersion in `build.sbt` was changed to 2.13 so that the Scala3-migrate tool (details [here](https://docs.scala-lang.org/scala3/guides/migration/scala3-migrate.html)) could be used.

Prior to using the migration tool, I realised that `org.apache.commons.lang3.StringEscapeUtils` was depreciated and replaced it with `org.apache.commons.text.StringEscapeUtils` in `Lexer.scala`. I also changed the sbtVersion to 1.8.2 from 1.3.12 as the migration tool to be used required the version to be at least 1.5.

There was a warning that stated that the flag `-Ypartial-unification` was not able to be parsed. This is because from Scala 2.13 onwards, the flag is enabled by default. Therefore, I removed the flag from `build.sbt`.

There was also another warning that said that at Line 37 of `Parser.scala`, the match may not be exhaustive. I found that removing the "if" clause in Line 39 resolved this as it generalised the failure condition.

I installed the migration tool afterwards by adding it as a plugin in `project/plugins.sbt`.

According to the migration guide, I first ran `migrate-libs scalangj` and discovered that the following libraries were not compatible with Scala 3:
| Library                  | Old Version | New Version |
|--------------------------|-------------|-------------|
| scala-parser-combinators | 1.1.2       | 2.3.0       |
| scalactic                | 3.0.8       | 3.2.9       |
| scalatest                | 3.0.8       | 3.2.9       |
| paiges-core              | 0.3.0       | 0.4.2       |

After changing the libraries to the new versions, I used `migrate-scalacOptions scalangj`. The migration tool stated that the `-Yrangepos` flag is not available in Scala 3. However, I had left it in the `build.sbt` file as the migration guide mentioned that we do not need to remove it.

I received an error in `Lexer.scala` after this, and it was due to this line
```scala
import scala.collection.convert.DecorateAsJava
```
As I could not find any methods that used this import, I removed it.

When I tried to run `migrate-syntax scalangj`, I got multiple errors in the test files. These stemmed from the change in the `scalatest` library version. I changed the line
```scala
import org.scalatest.{FunSuite, Matchers}
```
to
```scala
import org.scalatest.{funsuite, matchers}
```

For all the test classes, I changed the `FunSuite` to `funsuite.AnyFunSuite` and `Matchers` to `matchers.should.Matchers`. After these changes, I was able to run `migrate-syntax scalangj` successfully.

However, I was not able to run `migrate` as the tool stated that the project could not be compiled. After changing the scalaVersion in `build.sbt` to 3.2.2, I discovered that there were multiple instances in `Parser.scala` where the compiler was unable to interpret the types, which mostly occurred in lambda parameters. One of the errors (in Line 868) occurred due to the case classes `TypeMethodCall` and `ClassMethodCall` not having the method `apply` being called. Warnings were also raised over other instanced where `apply` was not being called. There was also a warning in Line 35 where the case Success(_, _) would not be caught. This was fixed by removing the `if !rest.atEnd` portion of the second case.

## Fixed Issues
### TestParser13 fails
Turns out it has failed already in the original version. By writing new test cases for the lexer, it seems that it fails for any token that is made up of alphabets. After discussing my observations with Prof Kenny, he pointed out that it could be a problem with not taking whitespaces into account. 

He suggested splitting the tokens into two categories.

In Category 1, the tokens need to have a trailing whitespace if it is succeeded by another Category 1 token (e.g. for the New token and an Identifier token "var1", it should be "new var1" instead of "newvar1").

In Category 2, the tokens do not need to have a trailing whitespace. e.g. parenthesis, variable assignment operator

However, I chose to do a slightly different method, in which a greedy form of lexing was done. I modified the function that lexed Identifier tokens. This is such that it lexed any token that starts with a JavaLetter such as the New token ("new") or Boolean tokens ("true"/"false") in addition to Identifier tokens, which was done through pattern matching.

Note: This version is still in Scala 2.12