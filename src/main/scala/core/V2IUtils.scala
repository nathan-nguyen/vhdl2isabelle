package core

import sg.edu.ntu.hchen.VHDLParser.{Identifier_listContext, Subtype_indicationContext}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class VInfo(typeInfo: TypeInfo, defInfo: DefInfo)

object V2IUtils {

  def refine_IExp_on(e: IExp_con, valType: VValType, v2s: Boolean): IExp_con = {
    val newValType = if (v2s) {
      val tyString = valType.s
      require(tyString.endsWith("_vector"))
      VValType(tyString.substring(0, tyString.length - 7))
    } else {
      valType
    }
    val newBaseType = newValType.asInstanceOf[VBaseType]
    IExp_con(newBaseType, e.const, e.expKind)
  }

  def getValType(idef: IDef): VValType = idef match {
    case v: Variable => v.valType
    case s: Signal => s.valType
    case p: Port => p.valType
    case vl: Vl => vl match {
      case vl_v: Vl_v => vl_v.iVariable.valType
      case vnl: Vnl => throw VIErrorMsg(s"${vnl}")
    }
    case spl: SPl => spl match {
      case spl_s: SPl_s => spl_s.iSignal.valType
      case spl_p: SPl_p => spl_p.iPort.valType
      case spnl: SPnl => throw VIErrorMsg(s"${spnl}")
    }
  }

  // NOTE: we don't deal with "OTHERS" directly, but check the type mismatch and then change it
  def getCrhsFromExp(lhs_def: IDef, rhsExp: IExp): Crhs = {
    if (lhs_def.getExpKind == ExpVectorKind && rhsExp.expKind == ExpScalarKind) {
      val exp: IExp = rhsExp match {
        case exp_con: IExp_con => {
          val valType = getValType(lhs_def)
          refine_IExp_on(exp_con, valType, v2s = true)
        }
        case _ => rhsExp
      }
      exp.crhs_e_rhso
    } else if ((lhs_def.getExpKind == ExpVectorKind && rhsExp.expKind == ExpVectorKind)
      || (lhs_def.getExpKind == ExpScalarKind && rhsExp.expKind == ExpScalarKind)) {
      val exp: IExp = rhsExp match {
        case exp_con: IExp_con => {
          val valType = getValType(lhs_def)
          refine_IExp_on(exp_con, valType, v2s = false)
        }
        case _ => rhsExp
      }
      exp.crhs_e_rhse
    } else throw VIErrorMsg(s"${rhsExp}")
  }

  def getIdList(ctx: Identifier_listContext): List[String] = {
    val ids = for {
      id <- ctx.identifier()
    } yield id.getText.toLowerCase
    ids.toList
  }

  def selectedNameFromSubtypeInd(ctx: Subtype_indicationContext): VSelectedName = {
    val names = for {
      name <- ctx.selected_name()
    } yield VSelectedName(name)
    val head = names.head
    require(head.suffixList.isEmpty, "selectedNameFromSubtypeInd")
    head
  }

  // TODO
  def def__v_clhs(idef: V_IDef, range: Option[Discrete_range], sn: VSelectedName): V_clhs = idef match {
    case variable: Variable => {
      val v_lhs = range match {
        case None => Lhs_v(variable, sn)
        case Some(discrete_range) => Lhs_va(variable, discrete_range, sn)
      }
      Clhs_v(v_lhs)
    }
    case vl: Vl => Clhs_vr(vl)
    case _ => throw VIErrorMsg(s"${idef}")
  }

  // TODO
  def def__sp_clhs(idef: SP_IDef, range: Option[Discrete_range], sn: VSelectedName): SP_clhs = idef match {
    case signal: Signal => {
      val sp_s = SP_s(signal, sn)
      val sp_lhs = range match {
        case None => Lhs_s(sp_s, sn)
        case Some(discrete_range) => Lhs_sa(sp_s, discrete_range, sn)
      }
      Clhs_sp(sp_lhs)
    }
    case port: Port => {
      val sp_p = SP_p(port, sn)
      val sp_lhs = range match {
        case None => Lhs_s(sp_p, sn)
        case Some(discrete_range) => Lhs_sa(sp_p, discrete_range, sn)
      }
      Clhs_sp(sp_lhs)
    }
    case spl: SPl => Clhs_spr(spl)
    case _ => throw VIErrorMsg(s"${idef}")
  }

}
