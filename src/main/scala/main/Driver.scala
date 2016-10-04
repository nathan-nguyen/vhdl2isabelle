package main

import core.VInfo

object Driver {

  val userHome = System.getProperty("user.home")

  def getInFile(fileName: String) = s"${userHome}/VHDL2Isabelle/vhdl2isar/src/main/resources/files/${fileName}"

  def getOutDir = s"${userHome}/VHDL2Isabelle/VHDLModel/VHDLModel/"

  def preInfo: VInfo = {
    val preFile = getInFile("pre.vhd")
    val preTransfer = new VITran(preFile, None)
    preTransfer.preInfo
  }

  def main(args: Array[String]) {
    val inFile = getInFile("simple.vhd")
    val transfer = new VITran(inFile, Some(preInfo))
    transfer.dump(getOutDir)
  }

}
