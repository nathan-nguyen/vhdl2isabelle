package main

import core.VInfo

/**
  * Created by Hongxu Chen.
  */
object Driver {

  val userHome = System.getProperty("user.home")

  def getInFile(fileName: String) = s"${userHome}/VHDL2Isabelle/vhdl2isabelle/src/main/resources/files/${fileName}"

  def getOutDir = s"${userHome}/VHDL2Isabelle/VHDLModel/"

  def preInfo: VInfo = {
    val preFile = getInFile("pre.vhd")
    val preTransfer = new VITranslator(preFile, None)
    preTransfer.preInfo
  }

  def main(args: Array[String]) {
    val inFile = getInFile("sample.vhd")
    val transfer = new VITranslator(inFile, Some(preInfo))
    transfer.dump(getOutDir)
  }

}
