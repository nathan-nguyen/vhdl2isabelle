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

  def generateIVariable(id: String, iExpression: IExpression, vSubtypeIndication: VSubtypeIndication) = {
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    vVariableType match {
      case vBaseType: VBaseType => {
        val iVariable = IVariable(id, vBaseType, iExpression)
        defInfo += (id, iVariable)
      }
      case _ => handler(s"${vVariableType}")
    }
  }

  def generateIVariable(id: String, vExpressionOption: Option[VExpression], vSubtypeIndication: VSubtypeIndication) = {
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    vVariableType match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.getInitialValue(vExpressionOption)(defInfo)
          case vVectorType: VVectorType => vSubtypeIndication.getExplicitRangeOption match {
            case Some(vExplicitRange) => vVectorType.getInitialValue(id, vExplicitRange, vExpressionOption)(typeInfo)
            case None => {
              // This case happens when the array variable does not have fixed sized
              // (V2I-024)
              vVectorType.getNullValue()
            }
          }
        }
        val iVariable = IVariable(id, vBaseType, iExpression)
        defInfo += (id, iVariable)
      }
      case vCustomizedType : VCustomizedType => vCustomizedType match{
        case vSubtype : VSubtype => {
          val vSubtypeIndication = typeInfo.subtypeDeclarationMap(vSubtype)
          generateIDefinition(id, vExpressionOption, vSubtypeIndication)(defInfo, typeInfo) match{
            case iVariable : IVariable => defInfo += (id, iVariable)
            case iVl_Vnl : IVl_Vnl => defInfo += (id, iVl_Vnl)
            case _ => throw VIError
          }
        }
        case vRecordType : VRecordType => {
          val initVals = vRecordType.getInitialValue(typeInfo, vExpressionOption)(defInfo)
          val iVl_Vnl = IVl_Vnl.generate(id, initVals)(typeInfo, defInfo)
          defInfo +=(id, iVl_Vnl)
        }
        case vArrayType : VArrayType => {
          vArrayType.getInitialValue(id, typeInfo, vExpressionOption)(defInfo) match {
            case iVariable: IVariable => defInfo += (id, iVariable)
            case iVl_Vnl: IVl_Vnl => defInfo += (id, iVl_Vnl)
            case _ => throw VIError
          }
        }
      }
    }
  }

  def generateIPort(id: String, vExpressionOption: Option[VExpression], vSubtypeIndication: VSubtypeIndication, portMode: PortMode.Value, portConnection: PortConnection.Value): Unit = {
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    vVariableType match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.getInitialValue(vExpressionOption)(defInfo)
          case vVectorType: VVectorType => vSubtypeIndication.getExplicitRangeOption match {
            case Some(vExplicitRange) => {
              vVectorType.getInitialValue(id, vExplicitRange, vExpressionOption)(typeInfo)
            }
            case None => ???
          }
        }
        val port = Port_baseType(id, vBaseType, iExpression, portMode, portConnection)
        defInfo +=(id, port)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.getInitialValue(typeInfo, vExpressionOption)(defInfo)
          val spnl = ISpl_Spnl(id, initVals, portMode, portConnection)(typeInfo, defInfo)
          defInfo +=(id, spnl)
        }
        case arrayType :VArrayType => handler(s"${arrayType}")
      }
    }
  }

  def genISignal(name: String, vSubtypeIndication: VSubtypeIndication, signalKind: SignalKind.Value): Unit = {
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    vVariableType match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.guessInitialValue
          case vVectorType: VVectorType => {
            vSubtypeIndication.getExplicitRangeOption match {
              case Some(vExplicitRange) => vVectorType.guessInitialValue(vExplicitRange)
              case None => ???
            }
          }
        }
        val signal = Signal(name, vBaseType, iExpression, signalKind)
        defInfo +=(name, signal)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.guessInitVals(typeInfo)
          val iSpl_Spnl = ISpl_Spnl(name, initVals, signalKind)
          defInfo +=(name, iSpl_Spnl)
        }
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }

  // [TN] This method is called when trying to get subtype definition
  def generateIDefinition(id: String, vExpressionOption: Option[VExpression], subtypeIndication: VSubtypeIndication)(defInfo: DefInfo, vTypeInfo: VTypeInfo) : V_IDef = {
    val vVariableType = VVariableType(subtypeIndication.getSimpleName)
    vVariableType match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.getInitialValue(vExpressionOption)(defInfo)
          case vVectorType: VVectorType => subtypeIndication.getExplicitRangeOption match {
            case Some(vExplicitRange) => vVectorType.getInitialValue(id, vExplicitRange, vExpressionOption)(typeInfo)
            case None => ???
          }
        }
        IVariable(id, vBaseType, iExpression)
      }
      case vCustomizedType : VCustomizedType => vCustomizedType match{
        case subtype : VSubtype => {
          val subtypeIndication = vTypeInfo.subtypeDeclarationMap(subtype)
          generateIDefinition(id, vExpressionOption, subtypeIndication)(defInfo, vTypeInfo)
        }
        case vRecordType : VRecordType => {
          val initVals = vRecordType.getInitialValue(vTypeInfo, vExpressionOption)(defInfo)
          IVl_Vnl.generate(id, initVals)(typeInfo, defInfo)
        }
        case vArrayType : VArrayType => handler(s"${vArrayType}")
      }
    }
  }
}