package main

import java.io.File
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{ANTLRFileStream, CommonTokenStream, Lexer, Token}
import org.slf4j.LoggerFactory
import core._
import sg.edu.ntu.vhdl2isabelle.{VHDLLexer, VHDLParser}
import utils.PErrorListener

/**
  * Created by Hongxu Chen.
  */
class VITranslator(inFile: String, vInfo: Option[VInfo]) {

  val logger = LoggerFactory.getLogger(getClass)

  val filePath = Paths.get(inFile)
  require(Files.exists(filePath), s"${filePath} not exists")

  val moduleName = {
    val vhdlName = {
      val fileName = filePath.getFileName.toString
      val pos = fileName.lastIndexOf(".")
      if (pos > 0) fileName.substring(0, pos) else fileName
    }
    s"VHDL2Isabelle_${vhdlName}"
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

  def _process(): Keeper = {
    val tree = parser.design_file()
    val listener = new TListener(vInfo)
    val walker = new ParseTreeWalker
    walker.walk(listener, tree)
    listener
  }

  def preInfo: VInfo = {
    val keeper = _process()
    VInfo(keeper.typeInfo, keeper.defInfo)
  }

  def dump(outDir: String) = {
    val keeper = _process()
    val outFile = outDir + s"${moduleName}.thy"
    val file = new File(outFile)
    if (keeper.definedEntities.nonEmpty) {
      for (entity <- keeper.definedEntities) {
        val exportRawName = s"${moduleName}_${entity}"
        printToFile(file) { p =>
          p.println(header)
          p.println("begin\n")
          p.println(keeper.defInfo)
          p.println("\n\n")
          p.println(keeper.entity)
          p.println("\n\n")
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
    s"""export_code ${entityName} simulation init_state sim_arch trans_vhdl_desc_complex in ${target}
        |module_name ${moduleName} file \"${outFileName}${ext}\"""".stripMargin
  }
}
