package parsing

import parsing.V2IUtils._

////////////////////////////////////////////////////////////////////////////

final case class IData(itemId: String, valType: String, initVal: IExp)

////////////////////////////////////////////////////////////////////////////

/**
  * make it IExp since otherwise there is no "toIExp" for it
  * It actually acts as a VALUE in Isar
  * TODO see whether needed to add another VALUE class
  */
final case class IValue(isarType: String, initVal: String) extends IExp

sealed trait IExp {
  override def toString = this match {
    case IValue(isarType, initVal) => s"""(${isarType} ${initVal})"""
    case IExp_con(valType, const) => s"""(exp_con (${VHDLize(valType)}, ${const}))"""
    case IExp_var(valType, variable) => s"""(exp_var (${VHDLize(valType)}, ${variable}))"""
    case IExp_sig(valType, signal) => s"""(exp_sig ${signal})"""
    case IExp_prt(valType, port) => s"""(exp_prt ${port})"""
    case IUexp(op, e) => s"""(uexp ${op} ${e})"""
    case IBexpl(e1, lop, e2) => s"""(bexpl ${e1} ${lop} ${e2})"""
    case IBexpr(e1, rop, e2) => s"""(bexpr ${e1} ${rop} ${e2})"""
    case IBexps(e1, sop, e2) => s"""(bexps ${e1} ${sop} ${e2})"""
    case IBexpta(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IBexpfa(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IExp_nth(e1, e2) => s"""(exp_nth ${e1} ${e2})"""
    case IExp_sl(e1, e2, e3) => s"""(exp_sl ${e1} ${e2} ${e3})"""
    case IExp_tl(e1, e2) => s"""(exp_tl ${e1} ${e2})"""
    case IExp_trl(e) => s"""(exp_trl ${e})"""
  }
}

case class IExp_con(valType: String, const: IValue) extends IExp

case class IExp_var(valType: String, variable: IValue) extends IExp

case class IExp_sig(valType: String, signal: Signal) extends IExp

case class IExp_prt(valType: String, port: Port) extends IExp

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

case class IExp_tl(e1: IExp, e2: IExp) extends IExp

case class IExp_trl(e: IExp) extends IExp