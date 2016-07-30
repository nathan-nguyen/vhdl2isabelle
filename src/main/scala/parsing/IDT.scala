package parsing

import parsing.V2IUtils.VHDLize

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SP_clhs

case class Clhs_sp(sp_clhs: SP_lhs) extends SP_clhs

case class Clhs_spr(spl: SPl) extends SP_clhs

/////////////////////////////////////////////////////////////////////////////////

case class Sensitivity_list()

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Discrete_range

case class VHDL_dis_to(l:IExp, r:IExp) extends Discrete_range

case class VHDL_dis_downto(l:IExp, r:IExp) extends Discrete_range

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SP_lhs

case class Lhs_s(sigPrt: ISigPrt) extends SP_lhs

case class Lhs_sa(sigPrt: ISigPrt, discreteRange: Discrete_range) extends SP_lhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SPl

case class SPl_s(iSignal: ISignalScalarDef) extends SPl

case class SPl_p(iPortDef: IPortScalarDef) extends SPl

case class SPnl(id: String, splList: List[SPl]) extends SPl

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class V_lhs

case class Lhs_v(variable: IVariable) extends V_lhs

case class LHs_va(variable: IVariable, discreteRange: Discrete_range) extends V_lhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Asmt_rhs

case class Rhs_e(exp: IExp) extends Asmt_rhs

case class Rhs_o(exp: IExp, discreteRange: Discrete_range) extends Asmt_rhs

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Seq_stmt

case class Sst_sa(id: String, sP_lhs: SP_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_va(id: String, v_lhs: V_lhs, asmt_rhs: Asmt_rhs) extends Seq_stmt

case class Sst_if(id: String, cond: IExp, then_seq_stmtList: List[Seq_stmt], else_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_l(id: String, cond: IExp, body_seq_stmtList: List[Seq_stmt]) extends Seq_stmt

case class Sst_n(id: String, cond: IExp) extends Seq_stmt

case class Sst_e(id: String, cond: IExp) extends Seq_stmt

case object Sst_nl extends Seq_stmt

/////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////

sealed abstract class Vl {
  // TODO deeper flatten?
  override def toString = this match {
    case Vl_v(iVarScalarDef) => s"vl_v ${iVarScalarDef.id}"
    case Vnl(id, vlList) => s"vlist_of_v ${id}"
  }
}

case class Vl_v(iVarScalarDef: IVarScalarDef) extends Vl

case class Vnl(id: String, vlList: List[Vl]) extends Vl

/////////////////////////////////////////////////////////////////////////////////

abstract class IDef

sealed abstract class IVarDef extends IDef

case class IVarScalarDef(id: String, valType: String, iExp: IExp) extends IVarDef {
  override def toString = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""vl_v (''${id}'', ${VHDLize(valType)}, ${iExp})"""

  def as_v: String = id
}

case class IVarListDef(id: String, iVals: List[IScalarOrVecIval]) extends IVarDef {
  override def toString = {
    val varDefs = for {
      iVal <- iVals
    } yield IVarScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal)

    val itemsRepr = varDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"vl\" where
        |\"${id} ≡ vnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  def as_vlist: String = s"(vlist_of_vl ${id})"
}

//////////////////////////////////////////////////////////////////////////////////

sealed abstract class ISigPrt extends IDef {
  def as_sigprt: String
}

sealed abstract class IPortDef extends ISigPrt

case class IPortScalarDef(id: String, valType: String, iExp: IExp, mode: String, conn: String = "connected") extends IPortDef {
  override def toString = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_p (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})"""

  def as_sigprt: String = s"(sp_p ${id})"
}

case class IPortListDef(id: String, iVals: List[IScalarOrVecIval], mode: String, conn: String = "connected") extends IPortDef {
  override def toString = {
    val portDefs = for {
      iVal <- iVals
    } yield IPortScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal, mode, conn)
    val itemsRepr = portDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"spl\" where
        |\"${id} ≡ spnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  override def as_sigprt: String = s"(sp_p ${id})"

  def as_sp_lhs_s = s"(lhs_s ${as_sigprt})"
}

sealed abstract class ISignalDef extends ISigPrt

case class ISignalScalarDef(id: String, valType: String, iExp: IExp, signalKind: String = "register") extends ISignalDef {
  override def toString = {
    s"""definition ${id}:: \"signal\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_s (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def as_sigprt: String = s"(sp_s ${id})"
}

case class ISignalListDef(id: String, iVals: List[IScalarOrVecIval], signalKind: String = "register") extends ISignalDef {
  override def toString = {
    val signalDefs = for {
      iVal <- iVals
    } yield ISignalScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal, signalKind)
    val itemsRepr = signalDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"spl\" where
        |\"${id} ≡ spnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  override def as_sigprt: String = s"(sp_s ${id})"
}
