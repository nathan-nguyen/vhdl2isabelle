package core.isabellesyntax

import core._
import core.vhdlsyntax._

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

  override def toString : String
}

case class IConstS(isaType: String, initVal: String) extends IConst {
  override def toString = s"(${isaType} ${initVal})"
}

// IConstL -> to -> "var_list"
sealed abstract class IConstL extends IConst {
  val valType: VVectorType
}

case class IConstL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstL {
  override def toString = s"(val_list ${iConstList.ISABELLE_r})"
}

case class IConstL_gen(valType: VVectorType, length: String, rawVal: Char) extends IConstL {
  override def toString = s"(val_list ${generateInitialValue(valType, length, rawVal)})"
}

// IConstRL -> downto -> "var_rlist"
sealed abstract class IConstRL extends IConst {
  val valType: VVectorType
}

case class IConstRL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstRL {
  override def toString = s"(val_rlist ${iConstList.ISABELLE_r})"
}

case class IConstRL_gen(valType: VVectorType, length: String, rawVal: Char) extends IConstRL {
  override def toString = s"(val_rlist ${generateInitialValue(valType, length, rawVal)})"
}

sealed abstract class IConstCustomized extends IConst

sealed abstract class IConstRecord extends IConstCustomized

// This is used for nested record - Generate the initial value
case class IConstRecord_gen(valType: VRecordType) extends IConstRecord

// TODO: Add IConstRecord_raw() for the case initial values exist.

sealed abstract class IConstArray extends IConstCustomized

sealed abstract class IConstArrayL extends IConstArray

case class IConstArrayL_gen(valType: VArrayType, length: Int, const_original: IConst) extends IConstArrayL {
  val iVarChar = IConstS("val_c", s"(CHR '${10}')")
  override def toString = s"(val_list (vec_gen ${length} ${const_original}))"
}

sealed abstract class IConstArrayRL extends IConstArray

case class IConstArrayRL_gen(valType: VArrayType, length: Int, const_original: IConst) extends IConstArrayRL {
  override def toString = s"(val_rlist (vec_gen ${length} IConstArrayRL_gen))"
}

//********************************************************************************************************************//

sealed trait ExpKind {
  def isV = this match {
    case v: ExpVectorKind => true
    case _ => false
  }
}

// Scala
case object ExpScalarKind extends ExpKind

sealed abstract class ExpVectorKind extends ExpKind

// Vector to
case object ExpVectorKindTo extends ExpVectorKind

// Vector downto
case object ExpVectorKindDownTo extends ExpVectorKind

sealed abstract class ExpCustomizedKind extends ExpKind

case object ExpRecordKind extends ExpCustomizedKind

sealed abstract class ExpArrayKind extends ExpCustomizedKind

// Array to
case object ExpArrayKindTo extends ExpArrayKind

// Array downto
case object ExpArrayKindDownTo extends ExpArrayKind

case object ExpUnknownKind extends ExpKind

//********************************************************************************************************************//

sealed abstract class IExpression {
  val expKind: ExpKind

  def getVType: VTypeDefinition = getIDef.getVType

  def getIDef: IDef = this match {
    case vl_rhs: IExp_vl_rhs => vl_rhs.v
    case spl_rhs: IExp_spl_rhs => spl_rhs.sp
    case v: IExp_variable => v.variable
    case s: IExp_signal => s.signal
    case p: IExp_port => p.port
    case _ => throw VIError
  }

  override def toString = this match {
    case IExp_baseTypeConstant(baseType, const, _) => s"""(exp_con (${VHDLize(baseType)}, ${const}))"""

    case IExp_recordTypeConstant(recordType, const, _) => handler(s"${recordType}")
    case IExp_arrayTypeConstant(arrayType, const, _) => s"""(exp_con (vhdl_array, ${const}))"""

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
  def crhs_r(defInfo: DefInfo): Crhs_r = throw VIError

}

sealed abstract class IExp_constant extends IExpression {
  val const: IConst
}

case class IExp_baseTypeConstant(baseType: VBaseType, const: IConst, expKind: ExpKind) extends IExp_constant

sealed abstract class IExp_customizedTypeConstant extends IExp_constant

case class IExp_recordTypeConstant(recordType : VRecordType, const: IConst, expKind: ExpKind) extends IExp_customizedTypeConstant

case class IExp_arrayTypeConstant(arrayType : VArrayType, const: IConst, expKind: ExpKind) extends IExp_customizedTypeConstant

// For storing identifiers
// Different from Isabelle, it must be a defined "variable"
case class IExp_variable(variable: IVariable_old, expKind: ExpKind) extends IExpression

case class IExp_signal(signal: Signal, expKind: ExpKind) extends IExpression

case class IExp_port(port: Port, expKind: ExpKind) extends IExpression

case class IUnaryExpression(op: VUnaryOperator.Ty, e: IExpression) extends IExpression {
  val expKind: ExpKind = e.expKind
}

case class IBinaryLogicalExpression(e1: IExpression, op: VLogicOp.Ty, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryRelationalExpression(e1: IExpression, op: VRelationOp.Ty, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}\n${e2}, ${e2.expKind}")
  val expKind: ExpKind = ExpScalarKind
}

case class IBinaryShiftingExpression(e1: IExpression, op: VShiftOp.Ty, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

sealed abstract class IBinaryArithmeticExpression extends IExpression

case class IBinaryArithmeticFactorExpression(e1: IExpression, op: VMultiplyingOperator.Ty, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticPrimaryExpression(e1: IExpression, op: VDoubleStarOperator.Ty, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticTermExpression(e1: IExpression, op: VAddingOperator.Ty, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}, \n${e2}, ${e2.expKind}")
  // if (e1.expKind != e2.expKind) handleExpKindMismatch(e1, e2, s"${toString}")
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

case class IExp_nth(e: IExpression, nthExp: IExpression) extends IExpression {
  require(e.expKind.isV && nthExp.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpScalarKind
}

case class IExp_sl(e: IExpression, e1: IExpression, e2: IExpression) extends IExpression {
  // FIXME: not sure whether "DT" or "T": (1) rely on "e" (2) order of "e1", "e2"
  require(e.expKind.isV && e1.expKind == ExpScalarKind && e2.expKind == ExpScalarKind)
  val expKind: ExpKind = e.expKind
}

case class IExp_tl(e: IExpression) extends IExpression {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindTo
}

case class IExp_trl(e: IExpression) extends IExpression {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindDownTo
}

// Fake IExp to convert vl/spl to IExp

case class IExp_vl_rhs(v: V_IDef, sn: VSelectedName, expKind: ExpKind) extends IExpression

case class IExp_spl_rhs(sp: SP_IDef, sn: VSelectedName, expKind: ExpKind) extends IExpression