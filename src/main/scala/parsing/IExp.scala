package parsing

import parsing.V2I._

object IUop extends Enumeration {
  type Ty = Value
  val abs, not, neg, pos = Value
}

object ILop extends Enumeration {
  type Ty = Value
  val and = Value("[and]")
  val or = Value("[or]")
  val nand = Value("[nand]")
  val nor = Value("[nor]")
  val xor = Value("[xor]")
  val xnor = Value("[xnor]")
}

object IRop extends Enumeration {
  type Ty = Value
  val eq = Value("[=]")
  val neq = Value("['/=]")
  val lt = Value("[<]")
  val le = Value("[<=]")
  val gt = Value("[>]")
  val ge = Value("[>=]")
}

object ISop extends Enumeration {
  type Ty = Value
  val sll = Value("[sll]")
  val srl = Value("[srl]")
  val sla = Value("[sla]")
  val sra = Value("[sra]")
  val rol = Value("[rol]")
  val ror = Value("[ror]")
}

object IAop extends Enumeration {
  type Ty = Value
  val add = Value("[+]")
  val sub = Value("[-]")
  val concat = Value("[&]")
  val mul = Value("[*]")
  val div = Value("[/=]")
  val rem = Value("[mod]")
  val exp = Value("[**]")
}

sealed trait IVal

final case class ISignal(valType: String, iExp: IExp, signalKind: String) extends IVal {
}

final case class IPort(valType: String, mode: String, iExp: IExp, conn: String = "connected") extends IVal {
  override def toString = s"""(${valType}, mode_${mode}, ${conn}, ${iExp})"""

  def definition(id: String, idType: String) = {
    require(id.nonEmpty && idType.nonEmpty, "id and idType should not be empty")
    s"""definition ${id}:: \"${idType}\" where \"${id} ≡ (''${id}'', ${valType}, mode_${mode}, ${conn}, ${iExp})\""""
  }
}

final case class IVariable(valType: String, iExp: IExp) extends IVal with IExp {
  def repr = s"""(${VHDLize(valType)}, ${iExp})"""

  def definition(id: String, idType: String) = {
    require(id.nonEmpty && idType.nonEmpty, "id and idType should not be empty")
    s"""definition ${id}:: \"${idType}\" where \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${iExp})\""""
  }
}

////////////////////////////////////////////////////////////////////////////

sealed trait IExp {
  override def toString = this match {
    case IExp_con(const) => s"""(exp_con ${const})"""
    case IExp_var(variable) => s"""(exp_var ${variable})"""
    case IExp_sig(signal) => s"""(exp_sig ${signal})"""
    case IExp_prt(port) => s"""(exp_prt ${port})"""
    case IUexp(op, e) => s"""(uexp ${op} ${e})"""
    case IBexpl(e1, lop, e2) => s"""(bexpl ${e1} ${lop} ${e2})"""
    case IBexpr(e1, rop, e2) => s"""(bexpr ${e1} ${rop} ${e2})"""
    case IBexps(e1, sop, e2) => s"""(bexps ${e1} ${sop} ${e2})"""
    case IBexpa(e1, aop, e2) => s"""(bexpa ${e1} ${aop} ${e2})"""
    case IExp_nth(e1, e2) => s"""(exp_nth ${e1} ${e2})"""
    case IExp_sl(e1, e2, e3) => s"""(exp_sl ${e1} ${e2} ${e3})"""
    case IExp_tl(e1, e2) => s"""(exp_tl ${e1} ${e2})"""
    case IExp_trl(e) => s"""(exp_trl ${e})"""
  }
}

case class IExp_con(const: IVariable) extends IExp {
  override def toString = s"""(exp_con ${const})"""
}

case class IExp_var(variable: IVariable) extends IExp {
  override def toString = s"""(exp_var ${variable})"""
}

case class IExp_sig(signal: ISignal) extends IExp

case class IExp_prt(port: IPort) extends IExp

case class IUexp(op: IUop.Ty, e: IExp) extends IExp

case class IBexpl(e1: IExp, op: ILop.Ty, e2: IExp) extends IExp

case class IBexpr(e1: IExp, op: IRop.Ty, e2: IExp) extends IExp

case class IBexps(e1: IExp, op: ISop.Ty, e2: IExp) extends IExp

case class IBexpa(e1: IExp, op: IAop.Ty, e2: IExp) extends IExp

case class IExp_nth(e1: IExp, e2: IExp) extends IExp

case class IExp_sl(e1: IExp, e2: IExp, e3: IExp) extends IExp

case class IExp_tl(e1: IExp, e2: IExp) extends IExp

case class IExp_trl(e: IExp) extends IExp