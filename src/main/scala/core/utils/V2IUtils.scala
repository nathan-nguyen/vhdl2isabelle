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
      case ec: IExpression_constantBaseType => idef.getExpKind match {
        case ExpScalarKind => IExpression_constantBaseType(idef.getVType.asInstanceOf[VBaseType], ec.iConst, idef.getExpKind)
        case vk: ExpVectorKind => {
          require(tobeRefined.expKind.isV, s"${tobeRefined.expKind}:\t${tobeRefined}")
          vk match {
            case ExpVectorKindTo => ec.iConst match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => IExpression_constantBaseType(idef.getVType.asInstanceOf[VBaseType], ec.iConst, idef.getExpKind)
              case rl: IConstRL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                rl match {
                  case IConstRL_raw(_, iConstList) => {
                    IExpression_constantBaseType(valType, IConstL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstRL_gen(_, length, rawVal) => {
                    IExpression_constantBaseType(valType, IConstL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
            }
            case ExpVectorKindDownTo => ec.iConst match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                l match {
                  case IConstL_raw(_, iConstList) => {
                    IExpression_constantBaseType(idef.getVType.asInstanceOf[VBaseType], IConstRL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstL_gen(_, length, rawVal) => {
                    IExpression_constantBaseType(idef.getVType.asInstanceOf[VBaseType], IConstRL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
              case rl: IConstRL => IExpression_constantBaseType(idef.getVType.asInstanceOf[VBaseType], rl, idef.getExpKind)
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
