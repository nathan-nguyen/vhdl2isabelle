package parsing

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime._
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

object Driver {
  def curDir: String = {
    val src = s"src${File.separator}main${File.separator}scala"
    val dir = getClass.getName.replaceAll("\\.\\w+\\$$", "").replace('.', File.separatorChar)
    src + File.separator + dir + File.separator
  }


  def getFile(s: String) = {
    val fileName = curDir + "files" + File.separator + s
    require(Files.exists(Paths.get(fileName)), s"${fileName} not exist")
    fileName
  }

  def iterateTokens(lexer: Lexer) = {
    var token = lexer.nextToken()
    while (token.getType != Token.EOF) {
      println(s"${token.getText}\t${token.getType}")
      token = lexer.nextToken()
    }
  }

  def main(args: Array[String]) {
    println("=" * 80)
    val file = getFile("div32.vhd")
    //    val file = getFile("standard.vhd")
    //    val file = getFile("misc.vhd")
    val lexer = new VHDLLexer(new ANTLRFileStream(file))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    val t = parser.design_file()
    val visitor = new TVisitor
    visitor.visit(t)
  }

}
