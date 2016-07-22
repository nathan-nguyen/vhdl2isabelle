package parsing

import java.io.File

class LibSpec extends BaseSpec {
  val fileName = "standard.vhd"
  val absFileName = System.getProperty("user.home") + File.separator + s"src/vhdl_parser/src/main/scala/parsing/files/${fileName}"
  val transfer = new Transfer(absFileName)

  "standard" should "standard" in {
    val tree =  transfer.parser.design_file()
  }

}
