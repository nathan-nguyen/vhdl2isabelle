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

  def genIVariable(id: String, expOption: Option[VExp], sti: VSubtypeInd): Unit = {
    val valType = VValType(sti.getSimpleName)
    valType match {
      case bt: VBaseType => {
        val initVal = bt match {
          case st: VScalarType => st.getInitVal(expOption)(defInfo)
          case vt: VVectorType => sti.getRange match {
            case Some(r) => {
              vt.getInitVal(r, expOption)
            }
            case None => {
              vt.guessInitVal(defaultRangeV(s"ConstDecl vector ${sti}"))
            }
          }
        }
        val variable = Variable(id, bt, initVal)
        defInfo +=(id, variable)
      }
      case ct@VCustomizedType(s) => {
        val initVals = ct.getInitVals(typeInfo.typeDeclTbl, expOption)(defInfo)
        val vnl = Vnl.gen(id, initVals)
        defInfo +=(id, vnl)
      }
    }
  }

  def generateIPort(id: String, expOption: Option[VExp], sti: VSubtypeInd, mode: PortMode.Ty, conn: PortConn.Ty): Unit = {
    val valType = VValType(sti.getSimpleName)
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
      case ct: VCustomizedType => {
        val initVals = ct.getInitVals(typeInfo.typeDeclTbl, expOption)(defInfo)
        val spnl = Spnl_list.generateFromPort(id, initVals, mode, conn, typeInfo, defInfo)
        defInfo +=(id, spnl)
      }
    }
  }

  def genISignal(id: String, sti: VSubtypeInd, signalKind: SignalKind.Ty): Unit = {
    val valType = VValType(sti.getSimpleName)
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
      case ct: VCustomizedType => {
        val initVals = ct.guessInitVals(typeInfo.typeDeclTbl)
        val spnl = Spnl_list.genFromSignal(id, initVals, signalKind)
        defInfo +=(id, spnl)
      }
    }
  }

}
