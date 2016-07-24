package parsing

import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{ANTLRFileStream, CommonTokenStream}
import sg.edu.ntu.hchen.{VHDLLexer, VHDLParser}

class TImpl(file: String) {

  val filePath = Paths.get(file)
  require(Files.exists(filePath), s"${filePath} not exists")
  val thyName = {
    val fileName = filePath.getFileName.toString
    val pos = fileName.lastIndexOf(".")
    if (pos > 0) fileName.substring(0, pos) else fileName
  }

  val parser = {
    val lexer = new VHDLLexer(new ANTLRFileStream(filePath.toString))
    val tokens = new CommonTokenStream(lexer)
    val parser = new VHDLParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new PErrorListener)
    parser
  }

  def header: String = {

    s"theory vhdl_${thyName}\nimports Main vhdl_component vhdl_syntax_complex\n"
  }

  def foot(target: String = "OCaml"): String = {
//    val extMap = Map("OCaml" -> ".ml", "Haskell" -> ".hs")
//    val ext = extMap.get(target) match {
//      case Some(x) => x
//      case None => throw new IllegalArgumentException(s"${target} not supported")
//    }
//    val outFileName = s"${thyName}${ext}"
    s"export_code ${thyName} simulation init_state arch_state_power sim_arch trans_vhdl_desc_complex in ${target}\n" +
      s"module_name vhdl_${thyName} file vhdl_${thyName}\n"
  }


}
