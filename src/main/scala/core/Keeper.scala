package core

import core.isabellesyntax._
import core.vhdlsyntax._

import scala.collection.mutable

/**
  * Created by Hongxu Chen.
  */
abstract class Keeper(vInfo: Option[VInfo]) {

  var entity: IEntity = _

  val conc_stmt_complexes = mutable.ArrayBuffer.empty[IConc_stmt_complex]

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new VTypeInfo(vInfo.map(_.typeInfo))

  val definedEntities = mutable.ArrayBuffer.empty[String]

  def genIVariable(id: String, expOption: Option[VExpression], subtypeIndication: VSubtypeIndication): Unit = {
    val valType = VTypeDefinition(subtypeIndication.getSimpleName)
    valType match {
      case baseType: VBaseType => {
        val initVal = baseType match {
          case scalarType: VScalarType => scalarType.getInitVal(expOption)(defInfo)
          case vectorType: VVectorType => subtypeIndication.getRange match {
            case Some(r) => vectorType.getInitVal(r, expOption)
            case None => vectorType.guessInitVal(defaultRangeV(s"ConstDecl vector ${subtypeIndication}"))
          }
        }
        val variable = IVariable(id, baseType, initVal)
        defInfo += (id, variable)
      }
      case ct : VCustomizedType => ct match{
        case subtype : VSubtype => {
          val subtypeIndication = typeInfo.subtypeDeclarationMap(subtype)
          generateIDefinition(id, expOption, subtypeIndication, defInfo, typeInfo) match{
            case variable : IVariable => defInfo += (id, variable)
            case vnl : Vnl => defInfo += (id, vnl)
            case _ => throw VIError
          }
        }
        case recordType : VRecordType => {
          val initVals = recordType.getInitVals(typeInfo, expOption)(defInfo)
          val vnl = Vnl.gen(id, initVals)
          defInfo +=(id, vnl)
        }
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }

  def generateIPort(id: String, expOption: Option[VExpression], sti: VSubtypeIndication, portMode: PortMode.Value, portConnection: PortConnection.Value): Unit = {
    val valType = VTypeDefinition(sti.getSimpleName)
    valType match {
      case bt: VBaseType => {
        val initVal = bt match {
          case st: VScalarType => st.getInitVal(expOption)(defInfo)
          case vt: VVectorType => sti.getRange match {
            case Some(r) => {
              vt.getInitVal(r, expOption)
            }
            case None => {
              vt.guessInitVal(defaultRangeV(s"generateIPort ${sti}"))
            }
          }
        }
        val port = Port_baseType(id, bt, initVal, portMode, portConnection)
        defInfo +=(id, port)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.getInitVals(typeInfo, expOption)(defInfo)
          val spnl = ISpl_Spnl(id, initVals, portMode, portConnection, typeInfo)
          defInfo +=(id, spnl)
        }
        case arrayType :VArrayType => handler(s"${arrayType}")
      }
    }
  }

  def genISignal(id: String, sti: VSubtypeIndication, signalKind: SignalKind.Value): Unit = {
    val valType = VTypeDefinition(sti.getSimpleName)
    valType match {
      case bt: VBaseType => {
        val initVal = bt match {
          case st: VScalarType => st.guessInitVal
          case vt: VVectorType => {
            val range = sti.getRange.getOrElse(defaultRangeV(s"signalDecl vector ${sti}"))
            vt.guessInitVal(range)
          }
        }
        val signal = Signal(id, bt, initVal, signalKind)
        defInfo +=(id, signal)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.guessInitVals(typeInfo)
          val iSpl_Spnl = ISpl_Spnl(id, initVals, signalKind)
          defInfo +=(id, iSpl_Spnl)
        }
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }

  // [TN] This method is called when trying to get subtype definition
  def generateIDefinition(id: String, expOption: Option[VExpression], subtypeIndication: VSubtypeIndication, defInfo: DefInfo, typeInfo: VTypeInfo) : V_IDef = {
    val valType = VTypeDefinition(subtypeIndication.getSimpleName)
    valType match {
      case baseType: VBaseType => {
        val initVal = baseType match {
          case scalarType: VScalarType => scalarType.getInitVal(expOption)(defInfo)
          case vectorType: VVectorType => subtypeIndication.getRange match {
            case Some(r) => vectorType.getInitVal(r, expOption)
            case None => vectorType.guessInitVal(defaultRangeV(s"ConstDecl vector ${subtypeIndication}"))
          }
        }
        IVariable(id, baseType, initVal)
      }
      case ct : VCustomizedType => ct match{
        case subtype : VSubtype => {
          val subtypeIndication = typeInfo.subtypeDeclarationMap(subtype)
          generateIDefinition(id, expOption, subtypeIndication, defInfo, typeInfo)
        }
        case rt : VRecordType => {
          val initVals = rt.getInitVals(typeInfo, expOption)(defInfo)
          Vnl.gen(id, initVals)
        }
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }
}