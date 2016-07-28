package parsing

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{ANTLRFileStream, CommonTokenStream, Lexer, Token}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

class VITran(file: String) {

  val filePath = Paths.get(file)
  require(Files.exists(filePath), s"${filePath} not exists")

  val moduleName = {
    val vhdlName = {
      val fileName = filePath.getFileName.toString
      val pos = fileName.lastIndexOf(".")
      if (pos > 0) fileName.substring(0, pos) else fileName
    }
    s"VVV_${vhdlName}"
  }


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

  def transfer() = {
    val visitor = new TVisitor
    val tree = parser.design_file()
    visitor.visit(tree)
    val outFile = Utils.getOutFile(s"${moduleName}.thy")
    val file = new File(outFile)
    printToFile(file) { p =>
      p.println(header)
      p.println("begin\n")
      for {
        (id, d) <- visitor.defs
      } {
        p.println(d + "\n")
      }
      p.println(foot("OCaml"))
      p.println("\nend")
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
    s"theory ${moduleName}\nimports Main vhdl_component vhdl_syntax_complex"
  }

  def foot(target: String = "OCaml"): String = {
    val extMap = Map("OCaml" -> ".ml", "Haskell" -> ".hs", "Scala" -> ".scala", "SML" -> ".ml")
    val ext = extMap.get(target) match {
      case Some(x) => x
      case None => throw new IllegalArgumentException(s"${target} not supported")
    }
    val outFileName = s"${moduleName}${ext}"
    s"""export_code ${moduleName} simulation init_state arch_state_power sim_arch trans_vhdl_desc_complex in ${target}
        |module_name ${moduleName} file \"${outFileName}\"""".stripMargin
  }
}
