package main

object Driver {

  def getInFile(fileName: String) = "/home/hongxu/src/vhdl_parser/src/main/resources/files/" + fileName

  def getOutDir = "/home/hongxu/Dropbox/000/isabelle/VHDLModel/"

  def main(args: Array[String]) {
    val inFile = getInFile("simple.vhd")
    val transfer = new VITran(inFile, getOutDir)
    transfer.transfer()
  }

}
