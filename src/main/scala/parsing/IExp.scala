package parsing

import VIType._

sealed trait IUop

final class IAbs extends IUop

final class INot extends IUop

final class INeg extends IUop

final class IPos extends IUop

sealed trait IBop

class ILop extends IBop

final class IL_and extends ILop

final class IL_or extends ILop

final class IL_nand extends ILop

final class IL_nor extends ILop

final class IL_xor extends ILop

final class IL_xnor extends ILop

class IRop extends IBop

final class IR_eq extends IRop

final class IR_neq extends IRop

final class IR_lt extends IRop

final class IR_le extends IRop

final class IR_gt extends IRop

final class IR_ge extends IRop

class ISop extends IBop

final class IS_sll extends ISop

final class IS_srl extends ISop

final class IS_sla extends ISop

final class IS_sra extends ISop

final class IR_sol extends ISop

final class IR_sor extends ISop

class IAop extends IBop

final class IA_add extends IAop

final class IA_sub extends IAop

final class IA_concat extends IAop

final class IA_mul extends IAop

final class IA_div extends IAop

final class IA_rem extends IAop

final class IA_exp extends IAop

abstract class IVal(id:String, idType:String) {
  def definition = {
    require(id.nonEmpty && idType.nonEmpty, "id and idType should not be empty")
    s"""definition ${id}:: \"${idType}\" where \"${id} ≡ ${toString}\""""
  }
}

final case class ISignal(id:String, idType:String) extends IVal(id, idType)

final case class IPort(id: String, idType:String, valType: String, mode: String, expr:IExp, conn: String = "connected") extends IVal(id, idType) {
  override def toString = {
    s"""(''${id}'', ${valType}, mode_${mode}, ${conn}, ${expr})"""
  }
}

final case class IVariable(id: String, idType: String, valType: String, initVal: Option[String]) extends IVal(id, idType) {
  override def toString = {
    valType match {
      case "integer" => {
        val init = initVal match {
          case Some(v) => v
          case None => "0"
        }
        s"""(''${id}'', ${VHDLize(valType)}, (val_i ${init}))"""
      }
      case "real" => {
        val init = initVal match {
          case Some(v) => v
          case None => "0.0"
        }
        s"""(''${id}'', ${VHDLize(valType)}, (val_r ${init}))\""""
      }
      case "character" => {
        val init = initVal match {
          case Some(v) => v
          case None => "'0'"
        }
        s"""???"""
      }
      case "std_ulogic" => {
        val init = initVal match {
          case Some(v) => v
          case None => "'0'"
        }
        s"""(${VHDLize(valType)}, (val_c (CHR '${init}')))"""
      }
      case "std_logic" => {
        val init = initVal match {
          case Some(v) => v
          case None => "'0'"
        }
        s"""(${VHDLize(valType)}, (val_c (CHR '${init}')))"""
      }
      case "BOOLEAN" => {
        s"""???"""
      }
      case _ => s"[[TODO ${valType}]]"
    }
  }

}

sealed trait IExp {
  override def toString = this match {
    case IExp_con(const) => s"""(exp_con ${const})"""
    case IExp_var(variable) => s"""(exp_var ${variable})"""
    case IExp_sig(signal) => s"""???"""
    case IExp_prt(port) => s"""???"""
    case IUexp(op, e) => s"""???"""
    case IBexpl(e1, lop, e2) => s"""???"""
    case IBexpr(e1, rop, e2) => s"""???"""
    case IBexps(e1, sop, e2) => s"""???"""
    case IBexpa(e1, aop, e2) => s"""???"""
    case IExp_nth(e1, e2) => s"""???"""
    case IExp_sl(e1, e2, e3) => s"""???"""
    case IExp_tl(e1, e2) => s"""???"""
    case IExp_trl(e) => s"""???"""
  }
}

case class IExp_con(const: IVariable) extends IExp {
  override def toString = s"""(exp_con ${const})"""
}

case class IExp_var(variable: IVariable) extends IExp

case class IExp_sig(signal: ISignal) extends IExp

case class IExp_prt(port: IPort) extends IExp

case class IUexp(op: IUop, e: IExp) extends IExp

case class IBexpl(e1: IExp, op: ILop, e2: IExp) extends IExp

case class IBexpr(e1: IExp, op: IRop, e2: IExp) extends IExp

case class IBexps(e1: IExp, op: ISop, e2: IExp) extends IExp

case class IBexpa(e1: IExp, op: IAop, e2: IExp) extends IExp

case class IExp_nth(e1: IExp, e2: IExp) extends IExp

case class IExp_sl(e1: IExp, e2: IExp, e3: IExp) extends IExp

case class IExp_tl(e1: IExp, e2: IExp) extends IExp

case class IExp_trl(e: IExp) extends IExp