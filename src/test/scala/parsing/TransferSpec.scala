package parsing

class TransferSpec extends BaseSpec {

  val absFileName = Utils.getDataFile("simple.vhd")
  val transfer = new VITran(absFileName)

  val logVisitor = new TVisitor

  ignore should "have header" in {
    transfer.header should be("theory vhdl_simpl\nimports Main vhdl_component vhdl_syntax_complex\n")
  }

  "" should "parse" in {
    val tree = transfer.parser.design_file()
    logVisitor.visit(tree)

    //    logVisitor.defs("scantest") should be ("""definition scantest:: "variable" where "scantest ≡ (''scantest'', vhdl_integer, (val_i 0))"""")
    val portDefStr =
      """definition rst:: "port" where
        | "rst ≡ (''rst'', vhdl_std_ulogic, mode_in, connected, (exp_con (vhdl_std_ulogic, (val_c (CHR ''0'')))))"""".stripMargin
    logVisitor.defs("rst") should be(portDefStr)
  }

}
