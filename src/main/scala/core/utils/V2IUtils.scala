package core

import core.isabellesyntax._
import core.vhdlsyntax._
import sg.edu.ntu.vhdl2isabelle.VHDLParser.{Identifier_listContext, Subtype_indicationContext}

import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Created by Hongxu Chen.
  */
case class VInfo(typeInfo: VTypeInfo, defInfo: DefInfo)

object V2IUtils {

  // Change valType to correct one (character/std_logic/std_ulogic)
  def refine__valType(idef: IDef, tobeRefined: IExpression): IExpression = {
    tobeRefined match {
      case ec: IExp_baseTypeConstant => idef.getExpKind match {
        case ExpScalarKind => IExp_baseTypeConstant(idef.getVType.asInstanceOf[VBaseType], ec.const, idef.getExpKind)
        case vk: ExpVectorKind => {
          require(tobeRefined.expKind.isV, s"${tobeRefined.expKind}:\t${tobeRefined}")
          vk match {
            case ExpVectorKindTo => ec.const match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => IExp_baseTypeConstant(idef.getVType.asInstanceOf[VBaseType], ec.const, idef.getExpKind)
              case rl: IConstRL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                rl match {
                  case IConstRL_raw(_, iConstList) => {
                    IExp_baseTypeConstant(valType, IConstL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstRL_gen(_, length, rawVal) => {
                    IExp_baseTypeConstant(valType, IConstL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
            }
            case ExpVectorKindDownTo => ec.const match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                l match {
                  case IConstL_raw(_, iConstList) => {
                    IExp_baseTypeConstant(idef.getVType.asInstanceOf[VBaseType], IConstRL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstL_gen(_, length, rawVal) => {
                    IExp_baseTypeConstant(idef.getVType.asInstanceOf[VBaseType], IConstRL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
              case rl: IConstRL => IExp_baseTypeConstant(idef.getVType.asInstanceOf[VBaseType], rl, idef.getExpKind)
            }
          }
        }
        case expCustomizedKind : ExpCustomizedKind => handler (s"${ec}")
        case ExpUnknownKind => handler(s"${ec}")
      }
      case _ => tobeRefined
    }
  }

  def refine__valType(trusted: IExpression, tobeRefined: IExpression): IExpression = {
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

  // NOTE: [HC] We don't deal with "OTHERS" directly, but check the type mismatch and then change it
  def exp__Crhs(lhs_def: IDef, rhsExp: IExpression): Crhs = {
    // [HC] This itself does little, only to change the valType
    // [HC] "valType" is reliable, however may change to "scalarized"
    def refine__sv_inner(e: IExp_baseTypeConstant, valType: VBaseType): IExp_baseTypeConstant = {
      val newValType = valType.asInstanceOf[VVectorType].scalarize
      IExp_baseTypeConstant(newValType, e.const, e.expKind)
    }

    if (lhs_def.getExpKind.isV && rhsExp.expKind == ExpScalarKind) {
      val exp: IExpression = rhsExp match {
        case exp_con: IExp_baseTypeConstant => {
          val valType = lhs_def.getVType.asInstanceOf[VBaseType]
          refine__sv_inner(exp_con, valType)
        }
        case _ => rhsExp
      }
      exp.crhs_e_rhso
    } else if ((lhs_def.getExpKind.isV && rhsExp.expKind.isV)
      || (lhs_def.getExpKind == ExpScalarKind && rhsExp.expKind == ExpScalarKind)) {
      val exp: IExpression = rhsExp match {
        case exp_con: IExp_baseTypeConstant => {
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

}
