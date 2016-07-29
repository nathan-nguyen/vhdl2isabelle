package parsing

import parsing.V2I._

//////////////////////////////////////////////////////////////////////////////

sealed trait IDef {
}

sealed abstract class ISigPrt {
  def repr: String
}


sealed abstract class IVarDef extends IDef


case class IVarScalarDef(id: String, valType: String, iExp: IExp) extends IVarDef {
  override def toString = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""vl_v (''${id}'', ${VHDLize(valType)}, ${iExp})"""
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
}

sealed abstract class IPortDef extends ISigPrt

case class IPortScalarDef(id: String, valType: String, iExp: IExp, mode: String, conn: String = "connected") extends IPortDef {
  override def toString = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_p (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})"""

  override def repr: String = s"(sp_p ${id})"
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

  override def repr: String = s"(sp_p ${id})"
}

sealed abstract class ISignalDef extends ISigPrt

case class ISignalScalarDef(id: String, valType: String, iExp: IExp, signalKind: String = "register") extends ISignalDef {
  override def toString = {
    s"""definition ${id}:: \"signal\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_s (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def repr: String = s"(sp_s ${id})"
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

  override def repr: String = s"(sp_s ${id})"
}

////////////////////////////////////////////////////////////////////////////

/**
  * an IValue has name and
  * 1. an initial value (IExp) and type
  * 2. a list of initial value (IExp) and type
  */
sealed trait IValue

final case class IScalarOrVecIval(itemId: String, valType: String, initVal: IExp) extends IValue

final case class IListVal(itemId: String, valType: String) extends IValue

////////////////////////////////////////////////////////////////////////////

final case class ISignal(valType: String, iExp: IExp, signalKind: String)

final case class IPort(valType: String, mode: String, iExp: IExp, conn: String = "connected")

/**
  * make it IExp since otherwise there is no "toIExp" for it
  * It actually acts as a VALUE in Isar
  * TODO see whether needed to add another VALUE class
  */
final case class IVariable(isarType: String, initVal: String) extends IExp

sealed trait IExp {
  override def toString = this match {
    case IVariable(isarType, initVal) => s"""(${isarType} ${initVal})"""
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

case class IExp_con(valType: String, const: IVariable) extends IExp

case class IExp_var(valType: String, variable: IVariable) extends IExp

case class IExp_sig(valType: String, signal: ISignal) extends IExp

case class IExp_prt(valType: String, port: IPort) extends IExp

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