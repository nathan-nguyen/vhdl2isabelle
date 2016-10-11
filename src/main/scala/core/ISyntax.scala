package core

//********************************************************************************************************************//

sealed abstract class IConst {

  def genInitVal(valType: VVectorType, length: Int, rawVal: Char = '0'): String = {
    val s = valType.s
    val numericVal = s"'${rawVal}'"
    val genCmd = s.substring(0, s.length - vectorFlag.length) + "_vec_gen"
    val iVarChar = IConstS("val_c", s"(CHR '${numericVal}')")
    s"(${genCmd} ${length} ${iVarChar})"
  }

  override def toString = this match {
    case IConstS(isarType, initVal) => s"(${isarType} ${initVal})"
    case l: IConstL => l match {
      case IConstL_raw(valType, iConstList) => s"(val_list ${iConstList.ISAR_r})"
      case g@IConstL_gen(valType, length, rawVal) => s"(val_list ${genInitVal(valType, length, rawVal)})"
    }
    case l: IConstRL => l match {
      case IConstRL_raw(valType, iConstList) => s"(val_rlist ${iConstList.ISAR_r})"
      case g@IConstRL_gen(valType, length, rawVal) => s"(val_rlist ${genInitVal(valType, length, rawVal)})"
    }
  }
}

case class IConstS(isarType: String, initVal: String) extends IConst

sealed abstract class IConstL extends IConst {
  val valType: VVectorType
}

case class IConstL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstL

case class IConstL_gen(valType: VVectorType, length: Int, rawVal: Char) extends IConstL

abstract class IConstRL extends IConst {
  val valType: VVectorType
}

case class IConstRL_raw(valType: VVectorType, iConstList: List[IConst]) extends IConstRL

case class IConstRL_gen(valType: VVectorType, length: Int, rawVal: Char) extends IConstRL


//********************************************************************************************************************//

sealed trait ExpKind {
  def isV = this match {
    case v: ExpVectorKind => true
    case _ => false
  }
}

case object ExpScalarKind extends ExpKind

// FIXME should distinguish TO and DOWNTO
abstract class ExpVectorKind extends ExpKind

case object ExpVectorKindT extends ExpVectorKind

case object ExpVectorKindDT extends ExpVectorKind

case object ExpUnknownKind extends ExpKind

//********************************************************************************************************************//

sealed abstract class IExp {
  val expKind: ExpKind

  def getVType: VBaseType = getIDef.getVType

  def getIDef: IDef = this match {
    case vl_rhs: IExp_vl_rhs => vl_rhs.v
    case spl_rhs: IExp_spl_rhs => spl_rhs.sp
    case v: IExp_var => v.variable
    case s: IExp_sig => s.signal
    case p: IExp_prt => p.port
    case _ => ???
  }

  override def toString = this match {
    case IExp_con(baseType, const, _) => s"""(exp_con (${VHDLize(baseType)}, ${const}))"""
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

  // Implemented outside
  def crhs_r(defInfo: DefInfo): Crhs_r = ???

}

case class IExp_con(baseType: VBaseType, const: IConst, expKind: ExpKind) extends IExp

// For storing identifiers
// Different from Isabelle, it must be a defined "variable"
case class IExp_var(variable: Variable, expKind: ExpKind) extends IExp

case class IExp_sig(signal: Signal, expKind: ExpKind) extends IExp

case class IExp_prt(port: Port, expKind: ExpKind) extends IExp

case class IUexp(op: VUop.Ty, e: IExp) extends IExp {
  val expKind: ExpKind = e.expKind
}

// Logic
case class IBexpl(e1: IExp, op: VLogicOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

// Relation
case class IBexpr(e1: IExp, op: VRelationOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}\n${e2}, ${e2.expKind}")
  // if (e1.expKind != e2.expKind) handleExpKindMismatch(e1, e2, s"${toString}")
  val expKind: ExpKind = ExpScalarKind
}

// Shift
case class IBexps(e1: IExp, op: VShiftOp.Ty, e2: IExp) extends IExp {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

sealed abstract class IBexpa extends IExp

// Factor arithmetic
case class IBexpfa(e1: IExp, op: VFactorOp.Ty, e2: IExp) extends IBexpa {
  require(e1.expKind == e2.expKind)
  val expKind: ExpKind = e1.expKind
}

// Term arithmetic
case class IBexpta(e1: IExp, op: VTermOp.Ty, e2: IExp) extends IBexpa {
  require(e1.expKind == e2.expKind, s"\n${e1}, ${e1.expKind}, \n${e2}, ${e2.expKind}")
  // if (e1.expKind != e2.expKind) handleExpKindMismatch(e1, e2, s"${toString}")
  val expKind: ExpKind = e1.expKind
}

case class IExp_nth(e: IExp, nthExp: IExp) extends IExp {
  require(e.expKind.isV && nthExp.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpScalarKind
}

case class IExp_sl(e: IExp, e1: IExp, e2: IExp) extends IExp {
  // FIXME: not sure whether "DT" or "T": (1) rely on "e" (2) order of "e1", "e2"
  require(e.expKind.isV && e1.expKind == ExpScalarKind && e2.expKind == ExpScalarKind)
  val expKind: ExpKind = e.expKind
}

case class IExp_tl(e: IExp) extends IExp {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindT
}

case class IExp_trl(e: IExp) extends IExp {
  require(e.expKind == ExpScalarKind)
  val expKind: ExpKind = ExpVectorKindDT
}

// Fake IExp to convert vl/spl to IExp

case class IExp_vl_rhs(v: V_IDef, sn: VSelectedName, expKind: ExpKind) extends IExp

case class IExp_spl_rhs(sp: SP_IDef, sn: VSelectedName, expKind: ExpKind) extends IExp