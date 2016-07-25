package parsing

import parsing.V2I._

object IUop extends Enumeration {
  type Ty = Value
  val abs = Value("[abs]")
  val not = Value("[not]")
  val neg = Value("[-:]")
  val pos = Value("[+:]")
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

final case class ISignal(valType: String, iExp: IExp, signalKind: String) extends IVal

final case class IPort(valType: String, mode: String, iExp: IExp, conn: String = "connected") extends IVal

final case class IVariable(valType: String, iExp: IExp) extends IVal with IExp

//////////////////////////////////////////////////////////////////////////////

sealed trait IDef

case class IVarDef(id: String, valType: String, iExp: IExp) extends IDef {
  override def toString = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""(''${id}'', ${VHDLize(valType)}, ${iExp})"""
}

case class IVarListDef(id: String, varDefs: List[IVarDef]) extends IDef {
  override def toString = {
    val itemsRepr = varDefs.map(_.asItem).mkString("[\n ", ",\n", "\n]")
    s"""definition ${id}:: ≡ \"variable list\" where
        |\"${id} ≡ ${itemsRepr}
     """.stripMargin
  }
}

case class IPortDef(id: String, valType: String, iExp: IExp, mode: String, conn: String = "connected") extends IDef {
  override def toString = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""(${id}, ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})"""
}

case class IPortListDef(id: String, portDefs: List[IPortDef]) extends IDef {
  override def toString = {
    val itemsRepr = portDefs.map(_.asItem).mkString("[\n ", ",\n", "\n]")
    s"""definition ${id}:: ≡ \"port list\" where
        |\"${id} ≡ ${itemsRepr}
     """.stripMargin
  }
}

case class ISignalDef(id: String, valType: String, iExp: IExp, signalKind: String) extends IDef {
  override def toString = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""(${id}, ${VHDLize(valType)}, ${signalKind}, ${iExp})"""
}

case class ISignalListDef(id: String, signalDefs: List[ISignalDef]) extends IDef {
  override def toString = {
    val itemsRepr = signalDefs.map(_.asItem).mkString("[\n ", ",\n", "\n]")
    s"""definition ${id}:: ≡ \"signal list\" where
        |\"${id} ≡ ${itemsRepr}
     """.stripMargin
  }
}


////////////////////////////////////////////////////////////////////////////

sealed trait IExp {
  override def toString = this match {
    case ival: IVariable => s"""${ival}"""
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

case class IExp_con(const: IVariable) extends IExp

case class IExp_var(variable: IVariable) extends IExp

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