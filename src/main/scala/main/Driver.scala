package main

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime._
import parsing.{TVisitor, Utils}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

object Driver {

  def iterateTokens(lexer: Lexer) = {
    var token = lexer.nextToken()
    while (token.getType != Token.EOF) {
      println(s"${token.getText}\t${token.getType}")
      token = lexer.nextToken()
    }
  }

  def main(args: Array[String]) {
    println("=" * 80)
    val file = Utils.getDataFile("files/div32.vhd")
    val lexer = new VHDLLexer(new ANTLRFileStream(file))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    val t = parser.design_file()
    val visitor = new TVisitor
    visitor.visit(t)
  }

}
