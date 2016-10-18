package core

import scala.collection.mutable

/**
  * Created by Hongxu Chen.
  */
abstract class Keeper(vInfo: Option[VInfo]) {

  var entity: IEntity = _

  val conc_stmt_complexes = mutable.ArrayBuffer.empty[Conc_stmt_complex]

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new TypeInfo(vInfo.map(_.typeInfo))

  val definedEntities = mutable.ArrayBuffer.empty[String]

  def genIVariable(id: String, expOption: Option[VExp], subtypeIndication: VSubtypeIndication): Unit = {
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
        val variable = Variable(id, baseType, initVal)
        defInfo += (id, variable)
      }
      case ct : VCustomizedType => ct match{
        case rt : VRecordType => {
          val initVals = rt.getInitVals(typeInfo.typeDeclaration, expOption)(defInfo)
          val vnl = Vnl.gen(id, initVals)
          defInfo +=(id, vnl)
        }
        case subtype : VSubtype => {
          val subtypeIndication = typeInfo.subtypeDeclaration(subtype)
          generateIDefinition(id, expOption, subtypeIndication, defInfo, typeInfo) match{
            case variable : Variable => defInfo += (id, variable)
            case vnl : Vnl => defInfo += (id, vnl)
            case _ => throw VIError
          }
        }
      }
    }
  }

  def generateIPort(id: String, expOption: Option[VExp], sti: VSubtypeIndication, mode: PortMode.Ty, conn: PortConn.Ty): Unit = {
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
        val port = Port_BaseType(id, bt, initVal, mode, conn)
        defInfo +=(id, port)
      }
      case ct: VCustomizedType => ct match {
        case recordType : VRecordType => {
          val initVals = recordType.getInitVals(typeInfo.typeDeclaration, expOption)(defInfo)
          val spnl = Spnl_list.generateFromPort(id, initVals, mode, conn, typeInfo.typeDeclaration)
          defInfo +=(id, spnl)
        }
        case subtype : VSubtype => handler(s"${subtype}")
      }
    }
  }

  def genISignal(id: String, sti: VSubtypeIndication, signalKind: SignalKind.Ty): Unit = {
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
        case recordType : VRecordType => {
          val initVals = recordType.guessInitVals(typeInfo.typeDeclaration)
          val spnl = Spnl_list.genFromSignal(id, initVals, signalKind)
          defInfo +=(id, spnl)
        }
        case subtype : VSubtype => handler(s"${subtype}")
      }
    }
  }

  // This method is called when trying to get subtype definition
  def generateIDefinition(id: String, expOption: Option[VExp], subtypeIndication: VSubtypeIndication, defInfo: DefInfo, typeInfo: TypeInfo) : V_IDef = {
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
        Variable(id, baseType, initVal)
      }
      case ct : VCustomizedType => ct match{
        case rt : VRecordType => {
          val initVals = rt.getInitVals(typeInfo.typeDeclaration, expOption)(defInfo)
          Vnl.gen(id, initVals)
        }
        case subtype : VSubtype => {
          val subtypeIndication = typeInfo.subtypeDeclaration(subtype)
          generateIDefinition(id, expOption, subtypeIndication, defInfo, typeInfo)
        }
      }
    }
  }

}
