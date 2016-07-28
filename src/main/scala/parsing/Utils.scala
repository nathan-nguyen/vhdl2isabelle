package parsing

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

object Utils {

  def pFromStr(s: String): VHDLParser = {
    val lexer = new VHDLLexer(new ANTLRInputStream(s))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new PErrorListener)
    parser
  }

  def getInFile(fileName: String) = "/home/hongxu/src/vhdl_parser/src/main/resources/files/" + fileName

  def getOutFile(fileName: String) = "/home/hongxu/Dropbox/000/isabelle/VHDLModel/" + fileName

}
