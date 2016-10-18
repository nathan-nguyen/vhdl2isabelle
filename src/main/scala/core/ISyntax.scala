package core

/**
  * Created by Hongxu Chen.
  */
sealed abstract class IConst {

  def generateInitialValue(valType: VVectorType, length: String, rawVal: Char = '0'): String = {
    val s = valType.s
    val numericVal = s"'${rawVal}'"
    val genCmd = s.substring(0, s.length - vectorFlag.length) + "_vec_gen"
    val iVarChar = IConstS("val_c", s"(CHR '${numericVal}')")
    s"(${genCmd} ${length} ${iVarChar})"
  }

  override def toString = this match {
    case IConstS(isarType, initVal) => s"(${isarType} ${initVal})"
    case l: IConstL => l match {
      case IConstL_raw(valType, iConstList) => s"(val_list ${iConstList.ISABELLE_r})"
      case g@IConstL_gen(valType, length, rawVal) => s"(val_list ${generateInitialValue(valType, length, rawVal)})"
    }
    case l: IConstRL => l match {
      case IConstRL_raw(valType, iConstList) => s"(val_rlist ${iConstList.ISABELLE_r})"
      case g@IConstRL_gen(valType, length, rawVal) => s"(val_rlist ${generateInitialValue(valType, length, rawVal)})"
    }
    case c: IConstRecord => handler(s"${c}")
  }
}

case class IConstS(isarType: String, initVal: String) extends IConst

// IConstL -> to -> "var_list"
sealed abstract class IConstL extends IConst {
  val valType: VVectorType
}

case class IConstL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstL

case class IConstL_gen(valType: VVectorType, length: String, rawVal: Char) extends IConstL

// IConstRL -> downto -> "var_rlist"
sealed abstract class IConstRL extends IConst {
  val valType: VVectorType
}

case class IConstRL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstRL

case class IConstRL_gen(valType: VVectorType, length: String, rawVal: Char) extends IConstRL

sealed abstract class IConstRecord extends IConst {
  val valType: VRecordType
}

// TODO: Add IConstRecord_raw() for the case initial values exist.

// This is used for nested record - Generate the initial value
case class IConstRecord_gen(valType: VRecordType) extends IConstRecord

//********************************************************************************************************************//

sealed trait ExpKind {
  def isV = this match {
    case v: ExpVectorKind => true
    case _ => false
  }
}

// Scala
case object ExpScalarKind extends ExpKind

abstract class ExpVectorKind extends ExpKind

// Vector to
case object ExpVectorKindTo extends ExpVectorKind

// Vector downto
case object ExpVectorKindDownTo extends ExpVectorKind

case object ExpRecordKind extends ExpKind

case object ExpUnknownKind extends ExpKind

//********************************************************************************************************************//

sealed abstract class IsabelleExpression {
  val expKind: ExpKind

  def getVType: VTypeDefinition = getIDef.getVType

  def getIDef: IDef = this match {
    case vl_rhs: IExp_vl_rhs => vl_rhs.v
    case spl_rhs: IExp_spl_rhs => spl_rhs.sp
    case v: IExp_variable => v.variable
    case s: IExp_signal => s.signal
    case p: IExp_port => p.port
    case _ => ???
  }

  override def toString = this match {
    case IExp_constant(baseType, const, _) => s"""(exp_con (${VHDLize(baseType)}, ${const}))"""
    case IExp_RecordConstant(recordType, const, _) => throw VIError
    case IExp_variable(variable, _) => s"""(exp_var ${variable.getId})"""
    case IExp_signal(signal, _) => s"""(exp_sig ${signal.getId})"""
    case IExp_port(port, _) => s"""(exp_prt ${port.getId})"""

    case IUnaryExpression(op, e) => s"""(uexp ${op} ${e})"""
    case IBinaryLogicalExpression(e1, lop, e2) => s"""(bexpl ${e1} ${lop} ${e2})"""
    case IBinaryRelationalExpression(e1, rop, e2) => s"""(bexpr ${e1} ${rop} ${e2})"""
    case IBinaryShiftingExpression(e1, sop, e2) => s"""(bexps ${e1} ${sop} ${e2})"""

    case IBinaryArithmeticTermExpression(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IBinaryArithmeticFactorExpression(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IBinaryArithmeticPrimaryExpression(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""

    case IExp_nth(e, nth) => s"""(exp_nth ${e} ${nth})"""
    case IExp_sl(e, e1, e2) => s"""(exp_sl ${e} ${e1} ${e2})"""
    case IExp_tl(e) => s"""(exp_tl ${e})"""
    case IExp_trl(e) => s"""(exp_trl ${e})"""

    case IExp_vl_rhs(vl, selectedName, _) => s"""(exp_of_vl ${selectedName.isa_v})"""
    case IExp_spl_rhs(spl, selectedName, _) => s"""(exp_of_spl ${selectedName.isa_sp})"""
  }

  def crhs_e_rhse: Crhs_e = Crhs_e(Rhs_e(this))

  def crhs_e_rhso: Crhs_e = Crhs_e(Rhs_o(this))

  // Implemented outside
  def crhs_r(defInfo: DefInfo): Crhs_r = ???

}

case class IExp_constant(baseType: VBaseType, const: IConst, expKind: ExpKind) extends IsabelleExpression

case class IExp_RecordConstant(recordType: VRecordType, const: IConst, expKind: ExpKind) extends IsabelleExpression

// For storing identifiers
// Different from Isabelle, it must be a defined "variable"
case class IExp_variable(variable: Variable, expKind: ExpKind) extends IsabelleExpression

case class IExp_signal(signal: Signal, expKind: ExpKind) extends IsabelleExpression

case class IExp_port(port: Port, expKind: ExpKind) extends IsabelleExpression

case class IUnaryExpression(op: VUnaryOperator.Ty, e: IsabelleExpression) extends IsabelleExpression {
  val expKind: ExpKind = e.expKind
}

case class IBinaryLogicalExpression(e1: IsabelleExpression, op: VLogicOp.Ty, e2: IsabelleExpression) extends IsabelleExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryRelationalExpression(e1: IsabelleExpression, op: VRelationOp.Ty, e2: IsabelleExpression) extends IsabelleExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}\n${e2}, ${e2.expKind}")
  // if (e1.expKind != e2.expKind) handleExpKindMismatch(e1, e2, s"${toString}")
  val expKind: ExpKind = ExpScalarKind
}

case class IBinaryShiftingExpression(e1: IsabelleExpression, op: VShiftOp.Ty, e2: IsabelleExpression) extends IsabelleExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

sealed abstract class IBinaryArithmeticExpression extends IsabelleExpression

case class IBinaryArithmeticFactorExpression(e1: IsabelleExpression, op: VMultiplyingOperator.Ty, e2: IsabelleExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticPrimaryExpression(e1: IsabelleExpression, op: VDoubleStarOperator.Ty, e2: IsabelleExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticTermExpression(e1: IsabelleExpression, op: VAddingOperator.Ty, e2: IsabelleExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}, \n${e2}, ${e2.expKind}")
  // if (e1.expKind != e2.expKind) handleExpKindMismatch(e1, e2, s"${toString}")
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

case class IExp_nth(e: IsabelleExpression, nthExp: IsabelleExpression) extends IsabelleExpression {
  require(e.expKind.isV && nthExp.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpScalarKind
}

case class IExp_sl(e: IsabelleExpression, e1: IsabelleExpression, e2: IsabelleExpression) extends IsabelleExpression {
  // FIXME: not sure whether "DT" or "T": (1) rely on "e" (2) order of "e1", "e2"
  require(e.expKind.isV && e1.expKind == ExpScalarKind && e2.expKind == ExpScalarKind)
  val expKind: ExpKind = e.expKind
}

case class IExp_tl(e: IsabelleExpression) extends IsabelleExpression {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindTo
}

case class IExp_trl(e: IsabelleExpression) extends IsabelleExpression {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindDownTo
}

// Fake IExp to convert vl/spl to IExp

case class IExp_vl_rhs(v: V_IDef, sn: VSelectedName, expKind: ExpKind) extends IsabelleExpression

case class IExp_spl_rhs(sp: SP_IDef, sn: VSelectedName, expKind: ExpKind) extends IsabelleExpression