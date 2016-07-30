package main

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{ANTLRFileStream, CommonTokenStream, Lexer, Token}
import org.slf4j.LoggerFactory
import parsing.{IEnv, TVisitor, VInfo}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}
import utils.PErrorListener

class VITran(inFile: String, vInfo: Option[VInfo]) {

  val logger = LoggerFactory.getLogger(getClass)

  val filePath = Paths.get(inFile)
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

  def _process(): TVisitor = {
    val visitor = new TVisitor(vInfo)
    val tree = parser.design_file()
    visitor.visit(tree)
    visitor
  }

  def preInfo: VInfo = {
    val visitor = _process()
    VInfo(visitor.typeInfo, visitor.defInfo)
  }

  def dump(outDir: String) = {
    val visitor = _process()
    val env = IEnv(visitor.defInfo)
    val outFile = outDir + s"${moduleName}.thy"
    val file = new File(outFile)
    if (visitor.definedEntities.nonEmpty) {
      for (entity <- visitor.definedEntities) {
        val exportRawName = s"${moduleName}_${entity}"
        printToFile(file) { p =>
          p.println(header)
          p.println("begin\n")
          p.println(visitor.defInfo)
          p.println(foot(entity, exportRawName, "OCaml"))
          p.println("\nend")
        }
      }
    } else {
      logger.warn("no entity declared in this module")
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

  def foot(entityName: String, outFileName: String, target: String): String = {
    val extMap = Map("OCaml" -> ".ml", "Haskell" -> ".hs", "Scala" -> ".scala", "SML" -> ".ml")
    val ext = extMap.get(target) match {
      case Some(x) => x
      case None => throw new IllegalArgumentException(s"${target} not supported")
    }
    s"""export_code ${entityName} simulation init_state arch_state_power sim_arch trans_vhdl_desc_complex in ${target}
        |module_name ${moduleName} file \"${outFileName}\"""".stripMargin
  }
}