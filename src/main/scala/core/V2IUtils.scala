package core

import sg.edu.ntu.vhdl2isabelle.VHDLParser.{Identifier_listContext, Subtype_indicationContext}

import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Created by Hongxu Chen.
  */
case class VInfo(typeInfo: TypeInfo, defInfo: DefInfo)

object V2IUtils {

  /**
    * Change valType to correct one (character/std_logic/std_ulogic)
    */
  def refine__valType(idef: IDef, tobeRefined: IsabelleExpression): IsabelleExpression = {
    tobeRefined match {
      case ec: IExp_constant => idef.getExpKind match {
        case ExpScalarKind => IExp_constant(idef.getVType.asInstanceOf[VBaseType], ec.const, idef.getExpKind)
        case vk: ExpVectorKind => {
          require(tobeRefined.expKind.isV, s"${tobeRefined.expKind}:\t${tobeRefined}")
          vk match {
            case ExpVectorKindTo => ec.const match {
              case s: IConstS => handler(s"${s}")
              case c: IConstRecord => handler(s"${c}")
              case l: IConstL => IExp_constant(idef.getVType.asInstanceOf[VBaseType], ec.const, idef.getExpKind)
              case rl: IConstRL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                rl match {
                  case IConstRL_raw(_, iConstList) => {
                    IExp_constant(valType, IConstL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstRL_gen(_, length, rawVal) => {
                    IExp_constant(valType, IConstL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
            }
            case ExpVectorKindDownTo => ec.const match {
              case s: IConstS => handler(s"${s}")
              case c: IConstRecord => handler(s"${c}")
              case l: IConstL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                l match {
                  case IConstL_raw(_, iConstList) => {
                    IExp_constant(idef.getVType.asInstanceOf[VBaseType], IConstRL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstL_gen(_, length, rawVal) => {
                    IExp_constant(idef.getVType.asInstanceOf[VBaseType], IConstRL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
              case rl: IConstRL => IExp_constant(idef.getVType.asInstanceOf[VBaseType], rl, idef.getExpKind)
            }
          }
        }
        case ExpRecordKind => handler (s"${ec}")
        case ExpUnknownKind => handler(s"${ec}")
      }
      case _ => tobeRefined
    }
  }

  def refine__valType(trusted: IsabelleExpression, tobeRefined: IsabelleExpression): IsabelleExpression = {
    val idef = Try(trusted.getIDef)
    idef match {
      case Success(idefV) => {
        refine__valType(idefV, tobeRefined)
      }
      case Failure(e) => {
        logger.warn(s"trusted? ${trusted}")
        tobeRefined
      }
    }
  }

  // NOTE: we don't deal with "OTHERS" directly, but check the type mismatch and then change it
  def exp__Crhs(lhs_def: IDef, rhsExp: IsabelleExpression): Crhs = {
    // this itself does little, only to change the valType
    // "valType" is reliable, however may change to "scalarized"
    def refine__sv_inner(e: IExp_constant, valType: VBaseType): IExp_constant = {
      val newValType = valType.asInstanceOf[VVectorType].scalarize
      IExp_constant(newValType, e.const, e.expKind)
    }

    if (lhs_def.getExpKind.isV && rhsExp.expKind == ExpScalarKind) {
      val exp: IsabelleExpression = rhsExp match {
        case exp_con: IExp_constant => {
          val valType = lhs_def.getVType.asInstanceOf[VBaseType]
          refine__sv_inner(exp_con, valType)
        }
        case _ => rhsExp
      }
      exp.crhs_e_rhso
    } else if ((lhs_def.getExpKind.isV && rhsExp.expKind.isV)
      || (lhs_def.getExpKind == ExpScalarKind && rhsExp.expKind == ExpScalarKind)) {
      val exp: IsabelleExpression = rhsExp match {
        case exp_con: IExp_constant => {
          refine__valType(lhs_def, rhsExp)
        }
        case _ => rhsExp
      }
      exp.crhs_e_rhse
    } else handler(s"${rhsExp}")
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
    case _ => handler(s"${idef}")
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
    case _ => handler(s"${idef}")
  }

}
