package parsing

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream, ParserRuleContext}
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

}