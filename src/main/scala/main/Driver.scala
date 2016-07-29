package main

import parsing.{DefInfo, TypeInfo, VInfo}

object Driver {

  def getInFile(fileName: String) = "/home/hongxu/src/vhdl_parser/src/main/resources/files/" + fileName

  def getOutDir = "/home/hongxu/Dropbox/000/isabelle/VHDLModel/"

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
