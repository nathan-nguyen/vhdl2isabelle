package core

import core.V2IUtils._

////////////////////////////////////////////////////////////////////////////

sealed abstract class IConst

case class IConstS(isarType: String, initVal: String) extends IConst {
  override def toString = s"(${isarType} ${initVal})"
}

case class IConstL(iConstList: List[IConst]) extends IConst

case class IConstRL(iConstList: List[IConst]) extends IConst

////////////////////////////////////////////////////////////////////////////

sealed trait IExp {
  override def toString = this match {
    case IExp_con(valType, const) => const match {
      case IConstS(isarType, initVal) => s"""(exp_con (${VHDLize(valType)}, (${isarType} ${initVal})))"""
      case IConstL(iConstList) => s"""(exp_con (${VHDLize(valType)}, (val_list ${iConstList.ISAR_r})))"""
      case IConstRL(iConstList) => s"""(exp_con (${VHDLize(valType)}, (val_rlist ${iConstList.ISAR_r})))"""
    }
    case IExp_var(variable) => s"""(exp_var ${variable.getId})"""
    case IExp_sig(signal) => s"""(exp_sig ${signal.getId})"""
    case IExp_prt(port) => s"""(exp_prt ${port.getId})"""
    case IUexp(op, e) => s"""(uexp ${op} ${e})"""
    case IBexpl(e1, lop, e2) => s"""(bexpl ${e1} ${lop} ${e2})"""
    case IBexpr(e1, rop, e2) => s"""(bexpr ${e1} ${rop} ${e2})"""
    case IBexps(e1, sop, e2) => s"""(bexps ${e1} ${sop} ${e2})"""
    case IBexpta(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IBexpfa(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IExp_nth(e1, e2) => s"""(exp_nth ${e1} ${e2})"""
    case IExp_sl(e1, e2, e3) => s"""(exp_sl ${e1} ${e2} ${e3})"""
    case IExp_tl(e) => s"""(exp_tl ${e})"""
    case IExp_trl(e) => s"""(exp_trl ${e})"""
    case IExp_vl_rhs(vl, selectedName) => s"""(exp_of_vl ${selectedName.isar_v})"""
    case IExp_spl_rhs(spl, selectedName) => s"""(exp_of_spl ${selectedName.isar_sp})"""
  }

  def crhs_e(): Crhs_e = {
    // FIXME should consider Rhs_o
    val asmt_rhs = Rhs_e(this)
    Crhs_e(asmt_rhs)
  }

  def crhs_r(defInfo: DefInfo): Crhs_r = ???

}

case class IExp_con(valType: String, const: IConst) extends IExp

// for storing identifiers
// different from isar, it must be a defined "variable"
case class IExp_var(variable: Variable) extends IExp

case class IExp_sig(signal: Signal) extends IExp

case class IExp_prt(port: Port) extends IExp

case class IUexp(op: VUop.Ty, e: IExp) extends IExp

// logic
case class IBexpl(e1: IExp, op: VLogicOp.Ty, e2: IExp) extends IExp

// relation
case class IBexpr(e1: IExp, op: VRelationOp.Ty, e2: IExp) extends IExp

// shift
case class IBexps(e1: IExp, op: VShiftOp.Ty, e2: IExp) extends IExp

sealed abstract class IBexpa extends IExp

// factor arighmetic
final case class IBexpfa(e1: IExp, op: VFactorOp.Ty, e2: IExp) extends IBexpa

// term arighmetic
final case class IBexpta(e1: IExp, op: VTermOp.Ty, e2: IExp) extends IBexpa

case class IExp_nth(e1: IExp, e2: IExp) extends IExp

case class IExp_sl(e1: IExp, e2: IExp, e3: IExp) extends IExp

// TODO not used
case class IExp_tl(e: IExp) extends IExp

// TODO not used
case class IExp_trl(e: IExp) extends IExp

// fake IExp to convert vl/spl to IExp

case class IExp_vl_rhs(v: V_IDef, sn: VSelectedName) extends IExp

case class IExp_spl_rhs(v: SP_IDef, sn: VSelectedName) extends IExp