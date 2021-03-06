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
      case iExpression_constantBaseType: IExpression_constantVBaseType => idef.getExpKind match {
        case ExpScalarKind => IExpression_constantVBaseType(idef.getVType.asInstanceOf[VBaseType], iExpression_constantBaseType.iConst, idef.getExpKind)
        case expVectorKind: ExpVectorKind => {
          require(tobeRefined.expKind.isV, s"${tobeRefined.expKind}:\t${tobeRefined}")
          expVectorKind match {
            case ExpVectorKindTo => iExpression_constantBaseType.iConst match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => IExpression_constantVBaseType(idef.getVType.asInstanceOf[VBaseType], iExpression_constantBaseType.iConst, idef.getExpKind)
              case rl: IConstRL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                rl match {
                  case IConstRL_raw(_, iConstList) => {
                    IExpression_constantVBaseType(valType, IConstL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstRL_gen(_, length, rawVal) => {
                    IExpression_constantVBaseType(valType, IConstL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
            }
            case ExpVectorKindDownTo => iExpression_constantBaseType.iConst match {
              case s: IConstS => handler(s"${s}")
              case c: IConstCustomized => handler(s"${c}")
              case l: IConstL => {
                val valType = idef.getVType.asInstanceOf[VVectorType]
                l match {
                  case IConstL_raw(_, iConstList) => {
                    IExpression_constantVBaseType(idef.getVType.asInstanceOf[VBaseType], IConstRL_raw(valType, iConstList), idef.getExpKind)
                  }
                  case IConstL_gen(_, length, rawVal) => {
                    IExpression_constantVBaseType(idef.getVType.asInstanceOf[VBaseType], IConstRL_gen(valType, length, rawVal), idef.getExpKind)
                  }
                }
              }
              case rl: IConstRL => IExpression_constantVBaseType(idef.getVType.asInstanceOf[VBaseType], rl, idef.getExpKind)
            }
          }
        }
        case expCustomizedKind : ExpCustomizedKind => handler (s"${iExpression_constantBaseType}")
        case ExpUnknownKind => handler(s"${iExpression_constantBaseType}")
      }
      case _ => tobeRefined
    }
  }

  def refine__valType(trusted: IExpression, tobeRefined: IExpression): IExpression = {
    val iDef = Try(trusted.getIDef)
    iDef match {
      case Success(idefV) => refine__valType(idefV, tobeRefined)
      case Failure(e) => {
        //logger.warn(s"trusted? ${trusted}")
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

  def selectedNameFromVSubtypeIndication(ctx: Subtype_indicationContext): VSelectedName = {
    val names = for {
      name <- ctx.selected_name()
    } yield VSelectedName(name)
    val head = names.head
    require(head.suffixList.isEmpty, "selectedNameFromSubtypeInd")
    head
  }

  def isAllDigits(x: String): Boolean = x forall Character.isDigit

}
