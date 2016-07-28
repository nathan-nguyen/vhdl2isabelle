package main

import parsing.{Utils, VITran}

object Driver {

  def main(args: Array[String]) {
    val inFile = Utils.getInFile("simple.vhd")
    val transfer = new VITran(inFile)
    transfer.transfer()
  }

}
