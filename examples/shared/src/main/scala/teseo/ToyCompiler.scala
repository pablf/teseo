package teseo

import teseo.*

/**
  * Toy-compiler of a very simple imperative programming language
  * TODO
  */
object ToyCompiler {

    type Memory = Array[Boolean]
    type Position = Int
    

    // Classes for the AST
    sealed trait AST
    case class Change() extends AST // change
    case class Remove() extends AST // remove
    case class Print() extends AST // print
    case class Pos(n: Int) extends AST // pos(n)
    case class If(isTrue: AST, isFalse: AST) extends AST // if isTrue, isFalse
    case class Import(name: String) extends AST // import name
    case class Export(code: AST, key: String) extends AST // export key {code}
    case class Goto(key: String) extends AST // key
    case class Module(code: AST)
    case class End() // end

    trait Compiler[A <: Conf] extends Algm[String, AST]

    object Compiler {
        //case class CompilerImpl[A <: Conf](alg: AlgmImpl[String, AST, A]) extends Compiler[A]

        val safe = ???//AlgmGen.pipeline[Compiler, Lexer.Lexer, Parser.Parser, Checker.Checker]
        val unsafe = ???//AlgmGen.pipeline[Compiler, Lexer.Lexer, Parser.Parser]
        
    }


    val sampleCode = ""

    

    object Lexer {

    trait Lexer[A <: Conf] extends Algm[String, AST]


    }

    object Parser {

        trait Parser[A <: Conf] extends Algm[String, AST]

    }

    object Checker {

        trait Checker[A <: Conf] extends Algm[String, AST]

    }



  
}



