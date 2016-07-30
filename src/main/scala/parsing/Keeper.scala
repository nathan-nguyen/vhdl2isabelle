package parsing

import org.slf4j.LoggerFactory
import parsing.V2IUtils._

import scala.collection.mutable

abstract class Keeper(vInfo:Option[VInfo]) {

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new TypeInfo(vInfo.map(_.typeInfo))

  val definedEntities = mutable.ArrayBuffer.empty[String]

  val logger = LoggerFactory.getLogger(classOf[TVisitor])

  def genIVariable(id: String, expOption: Option[VExp], sti: VSubtypeInd): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)
      val vnl = Vnl(id, initVals)
      defInfo +=(id, vnl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        // TODO perhaps not guess!!!
        sti.getRange match {
          case Some(r) => typeInfo._guessVectorInitVal(valType, r)
          case None => typeInfo._guessVectorInitVal(valType, defaultRange(s"ConstDecl vector ${sti}"))
        }
      } else {
        typeInfo.getScalarInitVal(valType, expOption)
      }
      val variable = IVariable(id, valType, initVal)
      defInfo +=(id, variable)
    }
  }

  def genIPort(id: String, expOption: Option[VExp], sti: VSubtypeInd, mode: PortMode.Ty, conn: PortConn.Ty): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)
      val spnl = SPnl(id, initVals, mode, conn)
      defInfo +=(id, spnl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"Port_list vector ${typeInfo}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        typeInfo.getScalarInitVal(valType, expOption)
      }
      val port = Port(id, valType, initVal, mode, conn)
      defInfo +=(id, port)
    }
  }

  def genISignal(id: String, sti: VSubtypeInd, signalKind: SignalKind.Ty): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo._guessListInitVals(valType)
      val spnl = SPnl(id, initVals, signalKind)
      defInfo +=(id, spnl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"signalDecl vector ${sti}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        typeInfo._guessScalarInitVal(valType)
      }
      val signal = Signal(id, valType, initVal, signalKind)
      defInfo +=(id, signal)
    }
  }

}
