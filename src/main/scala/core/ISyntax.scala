package core

////////////////////////////////////////////////////////////////////////////

sealed abstract class IConst

case class IConstS(isarType: String, initVal: String) extends IConst {
  override def toString = s"(${isarType} ${initVal})"
}

case class IConstL(iConstList: List[IConst]) extends IConst

case class IConstRL(iConstList: List[IConst]) extends IConst

////////////////////////////////////////////////////////////////////////////

sealed trait ExpKind

case object ExpScalarKind extends ExpKind

// FIXME should distinguish TO and DOWNTO
case object ExpVectorKind extends ExpKind

case object ExpUnknownKind extends ExpKind

////////////////////////////////////////////////////////////////////////////

sealed abstract class IExp {
  val expKind: ExpKind

  override def toString = this match {
    case IExp_con(baseType, const, _) => const match {
      case IConstS(isarType, initVal) => s"""(exp_con (${VHDLize(baseType)}, (${isarType} ${initVal})))"""
      case IConstL(iConstList) => s"""(exp_con (${VHDLize(baseType)}, (val_list ${iConstList.ISAR_r})))"""
      case IConstRL(iConstList) => s"""(exp_con (${VHDLize(baseType)}, (val_rlist ${iConstList.ISAR_r})))"""
    }
    case IExp_var(variable, _) => s"""(exp_var ${variable.getId})"""
    case IExp_sig(signal, _) => s"""(exp_sig ${signal.getId})"""
    case IExp_prt(port, _) => s"""(exp_prt ${port.getId})"""
    case IUexp(op, e) => s"""(uexp ${op} ${e})"""
    case IBexpl(e1, lop, e2) => s"""(bexpl ${e1} ${lop} ${e2})"""
    case IBexpr(e1, rop, e2) => s"""(bexpr ${e1} ${rop} ${e2})"""
    case IBexps(e1, sop, e2) => s"""(bexps ${e1} ${sop} ${e2})"""
    case IBexpta(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IBexpfa(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IExp_nth(e, nth) => s"""(exp_nth ${e} ${nth})"""
    case IExp_sl(e, e1, e2) => s"""(exp_sl ${e} ${e1} ${e2})"""
    case IExp_tl(e) => s"""(exp_tl ${e})"""
    case IExp_trl(e) => s"""(exp_trl ${e})"""
    case IExp_vl_rhs(vl, selectedName, _) => s"""(exp_of_vl ${selectedName.isar_v})"""
    case IExp_spl_rhs(spl, selectedName, _) => s"""(exp_of_spl ${selectedName.isar_sp})"""
  }

  def crhs_e_rhse: Crhs_e = Crhs_e(Rhs_e(this))

  def crhs_e_rhso: Crhs_e = Crhs_e(Rhs_o(this))

  // implemented outside
  def crhs_r(defInfo: DefInfo): Crhs_r = ???

}

case class IExp_con(baseType: VBaseType, const: IConst, expKind: ExpKind) extends IExp

// for storing identifiers
// different from isar, it must be a defined "variable"
case class IExp_var(variable: Variable, expKind: ExpKind) extends IExp

case class IExp_sig(signal: Signal, expKind: ExpKind) extends IExp

case class IExp_prt(port: Port, expKind: ExpKind) extends IExp

case class IUexp(op: VUop.Ty, e: IExp) extends IExp {
  val expKind: ExpKind = e.expKind
}

// logic
case class IBexpl(e1: IExp, op: VLogicOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind, s"${e1.expKind} ${e1}\n${e2.expKind} ${e2}")
  val expKind: ExpKind = e1.expKind
}

// relation
case class IBexpr(e1: IExp, op: VRelationOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = ExpScalarKind
}

// shift
case class IBexps(e1: IExp, op: VShiftOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

sealed abstract class IBexpa extends IExp

// factor arighmetic
case class IBexpfa(e1: IExp, op: VFactorOp.Ty, e2: IExp) extends IBexpa {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

// term arighmetic
case class IBexpta(e1: IExp, op: VTermOp.Ty, e2: IExp) extends IBexpa {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IExp_nth(e: IExp, nthExp: IExp) extends IExp {
  require(e.expKind == ExpVectorKind && nthExp.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpScalarKind
}

case class IExp_sl(e: IExp, e1: IExp, e2: IExp) extends IExp {
  require(e.expKind == ExpVectorKind && e1.expKind == ExpScalarKind && e2.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKind
}

case class IExp_tl(e: IExp) extends IExp {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKind
}

// TODO not used
case class IExp_trl(e: IExp) extends IExp {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKind
}

// fake IExp to convert vl/spl to IExp

case class IExp_vl_rhs(v: V_IDef, sn: VSelectedName, expKind: ExpKind) extends IExp

case class IExp_spl_rhs(v: SP_IDef, sn: VSelectedName, expKind: ExpKind) extends IExp