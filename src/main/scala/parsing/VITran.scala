package parsing

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{ANTLRFileStream, CommonTokenStream, Lexer, Token}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

class VITran(file: String) {

  val filePath = Paths.get(file)
  require(Files.exists(filePath), s"${filePath} not exists")

  val vhdlName = {
    val fileName = filePath.getFileName.toString
    val pos = fileName.lastIndexOf(".")
    if (pos > 0) fileName.substring(0, pos) else fileName
  }
  val moduleName = s"vhdl_${vhdlName}"

  val parser = {
    val lexer = new VHDLLexer(new ANTLRFileStream(filePath.toString))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new PErrorListener)
    parser
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def transfer = {
    val visitor = new TVisitor
    val tree = parser.design_file()
    visitor.visit(tree)
    val outFile = Utils.getDataFile(s"${moduleName}.thy")
    val file = new File(outFile)
    printToFile(file) { p =>
      p.println(header)
      for {
        (id, d) <- visitor.defs
      } {
        p.println(d + "\n")
      }
      p.println(foot("OCaml"))
    }
  }

  def iterateTokens(lexer: Lexer) = {
    var token = lexer.nextToken()
    while (token.getType != Token.EOF) {
      println(s"${token.getText}\t${token.getType}")
      token = lexer.nextToken()
    }
  }

  def header: String = {
    s"theory vhdl_${vhdlName}\nimports Main vhdl_component vhdl_syntax_complex\n"
  }

  def foot(target: String = "OCaml"): String = {
    val extMap = Map("OCaml" -> ".ml", "Haskell" -> ".hs", "Scala" -> ".scala", "SML" -> ".ml")
    val ext = extMap.get(target) match {
      case Some(x) => x
      case None => throw new IllegalArgumentException(s"${target} not supported")
    }
    val outFileName = s"${vhdlName}${ext}"
    s"""export_code ${vhdlName} simulation init_state arch_state_power sim_arch trans_vhdl_desc_complex in ${target}
        |module_name ${moduleName} file ${outFileName}\n""".stripMargin
  }
}
