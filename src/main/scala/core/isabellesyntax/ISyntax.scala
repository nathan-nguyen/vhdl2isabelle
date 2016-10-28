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

sealed abstract class IConstArrayTo extends IConstArray

case class IConstArrayTo_generate(valType: VArrayType, length: Int, const_original: IConst) extends IConstArrayTo {
  val iVarChar = IConstS("val_c", s"(CHR '${10}')")
  override def toString = s"(val_list (vec_gen ${length} ${const_original}))"
}

case class IConstArrayTo_initialValue(iVal: IVal) extends IConstArrayTo {
  override def toString = iVal.toString
}

sealed abstract class IConstArrayDownTo extends IConstArray

case class IConstArrayDownTo_generate(valType: VArrayType, length: Int, const_original: IConst) extends IConstArrayDownTo {
  override def toString = s"(val_rlist (vec_gen ${length} IConstArrayRL_gen))"
}

case class IConstArrayDownTo_initialValue(iVal: IVal) extends IConstArrayDownTo {
  override def toString = iVal.toString
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
    case v: IExpression_Variable => v.variable
    case s: IExp_signal => s.signal
    case p: IExp_port => p.port
    case _ => throw VIError
  }

  override def toString = this match {
    case IExpression_constantBaseType(baseType, const, _) => s"""(exp_con (${IType(baseType)}, ${const}))"""

    case IExpression_constantRecordType(recordType, const, _) => handler(s"${recordType}")
    case IExpression_constantArrayType(arrayType, const, _) => s"""(exp_con (vhdl_array, ${const}))"""

    case IExpression_Variable(iVariable, _) => s"""(exp_var ${iVariable.getName})"""
    case IExpression_Vl(iVl, _) => s"""(exp_r (rhsl_of_vl ${iVl.getName}))"""
    case IExp_signal(signal, _) => s"""(exp_sig ${signal.getName})"""
    case IExpression_Spl(iSpl: ISpl, _) => s"""(exp_r (rhsl_of_spl ${iSpl.getName}))"""
    case IExp_port(port, _) => s"""(exp_prt ${port.getName})"""

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

  def crhs_e_rhse: Crhs_e = Crhs_e(IAsmt_rhs_Rhs_e(this))

  def crhs_e_rhso: Crhs_e = Crhs_e(IAsmt_rhs_Rhs_o(this))

  // Implemented outside
  def crhs_r(defInfo: DefInfo): Crhs_r = throw VIError

}

sealed abstract class IExpression_constant extends IExpression {
  val iConst: IConst
}

case class IExpression_constantBaseType(baseType: VBaseType, iConst: IConst, expKind: ExpKind) extends IExpression_constant

sealed abstract class IExpression_constantCustomizedType extends IExpression_constant

case class IExpression_constantRecordType(recordType : VRecordType, iConst: IConst, expKind: ExpKind) extends IExpression_constantCustomizedType

case class IExpression_constantArrayType(arrayType : VArrayType, iConst: IConst, expKind: ExpKind) extends IExpression_constantCustomizedType

// For storing identifiers
// Different from Isabelle, it must be a defined "variable"
case class IExpression_Variable(variable: IVariable, expKind: ExpKind) extends IExpression

case class IExpression_Vl(iVl: IVl, expKind: ExpKind) extends IExpression

case class IExp_signal(signal: Signal, expKind: ExpKind) extends IExpression

case class IExpression_Spl(iSpl: ISpl, expKind: ExpKind) extends IExpression

case class IExp_port(port: Port, expKind: ExpKind) extends IExpression

case class IUnaryExpression(op: VUnaryOperator.Value, e: IExpression) extends IExpression {
  val expKind: ExpKind = e.expKind
}

case class IBinaryLogicalExpression(e1: IExpression, op: VLogicalOperator.Value, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryRelationalExpression(e1: IExpression, op: VRelationalOperator.Value, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}\n${e2}, ${e2.expKind}")
  val expKind: ExpKind = ExpScalarKind
}

case class IBinaryShiftingExpression(e1: IExpression, op: VShiftOperator.Value, e2: IExpression) extends IExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

sealed abstract class IBinaryArithmeticExpression extends IExpression

case class IBinaryArithmeticFactorExpression(e1: IExpression, op: VMultiplyingOperator.Value, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticPrimaryExpression(e1: IExpression, op: VDoubleStarOperator.Value, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

case class IBinaryArithmeticTermExpression(e1: IExpression, op: VAddingOperator.Value, e2: IExpression) extends IBinaryArithmeticExpression {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}, \n${e2}, ${e2.expKind}")
  val expKind: ExpKind = e1.expKind
}

//********************************************************************************************************************//

case class IExp_nth(iExpression: IExpression, nthExp: IExpression) extends IExpression {
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

//********************************************************************************************************************//

sealed abstract class IVal {

  override def toString = this match {
    case IVal_Val_i(i) => s"(val_i ${i})"
    case iVal_Val_r: IVal_Val_r => ???
    case iVal_Val_c: IVal_Val_c => ???
    case iVal_Val_b: IVal_Val_b => ???
    case iVal_Val_null: IVal_Val_null => "val_null"
    case IVal_Val_array(length, vExpression) => s"(vec_gen ${length} ${IVal(vExpression)})"
    case IVal_Val_list(iValList) => s"(val_list (${iValList.map(_.toListString).mkString("@")}))"
    case IVal_Val_rlist(iValList) => s"(val_rlist (${iValList.map(_.toListString).mkString("@")}))"
  }

  def toListString = this match {
    case iVal_Val_i: IVal_Val_i => s"[${iVal_Val_i}]"
    case iVal_Val_r: IVal_Val_r => s"[${iVal_Val_r}]"
    case iVal_Val_c: IVal_Val_c => s"[${iVal_Val_c}]"
    case iVal_Val_b: IVal_Val_b => s"[${iVal_Val_b}]"
    case iVal_Val_null: IVal_Val_null => s"[${iVal_Val_null}]"
    case iVal_Val_array: IVal_Val_array => s"(${iVal_Val_array})"
    case _ => ???
  }
}

object IVal {
  def apply(vExpression: VExpression): IVal = vExpression.getLiteral match {
    case Some(vLiteral) => vLiteral match {
      case VIntegerLiteral(s) => IVal_Val_i(s.toInt)
      case _ => ???
    }
    case None => ???
  }
}

case class IVal_Val_i(i: Int) extends IVal

case class IVal_Val_r() extends IVal

case class IVal_Val_c() extends IVal

case class IVal_Val_b() extends IVal

case class IVal_Val_null() extends IVal

// [TN] This type does not exist in Isabelle
case class IVal_Val_array(length: Int, vExpression: VExpression) extends IVal

case class IVal_Val_list(iValList: List[IVal]) extends IVal

case class IVal_Val_rlist(iValList: List[IVal]) extends IVal

//********************************************************************************************************************//

sealed abstract class IV_lhs {
  override def toString = this match {
    case IV_lhs_Lhs_v(iVariable, selectedName) => selectedName.suffixList match {
      // [TN]: The original value is selectedName.isa_v
      // However when selectedName.suffixList == Nil then name of the variable will be name IVariable.name
      case Nil => s"(lhs_v ${iVariable.name})"
      case _ => s"(lhs_v (var_of_vl ${selectedName.isa_v}))"
    }
    case IV_lhs_Lhs_va(iVariable, discreteRange, selectedName) => selectedName.suffixList match {
      case Nil => s"(lhs_va ${selectedName.isa_v} ${discreteRange})"
      case _ => s"(lhs_va (var_of_vl ${selectedName.isa_v}) ${discreteRange})"
    }
  }
}

case class IV_lhs_Lhs_v(variable: IVariable, selectedName: VSelectedName) extends IV_lhs

case class IV_lhs_Lhs_va(variable: IVariable, discreteRange: IDiscrete_range, selectedName: VSelectedName) extends IV_lhs