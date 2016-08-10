package core

import org.slf4j.LoggerFactory
import core.V2IUtils._

import scala.collection.mutable

abstract class Keeper(vInfo:Option[VInfo]) {

  var entity:IEntity = _

  val conc_stmt_complexes = mutable.ArrayBuffer.empty[Conc_stmt_complex]

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new TypeInfo(vInfo.map(_.typeInfo))

  val definedEntities = mutable.ArrayBuffer.empty[String]

  val logger = LoggerFactory.getLogger(this.getClass)

  def genIVariable(id: String, expOption: Option[VExp], sti: VSubtypeInd): Unit = {
    val valType = sti.getSimpleName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)(defInfo)
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
        typeInfo.getScalarInitVal(valType, expOption)(defInfo)
      }
      val variable = Variable(id, valType, initVal)
      defInfo +=(id, variable)
    }
  }

  def genIPort(id: String, expOption: Option[VExp], sti: VSubtypeInd, mode: PortMode.Ty, conn: PortConn.Ty): Unit = {
    val valType = sti.getSimpleName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)(defInfo)
      val spnl = SPnl(id, initVals, mode, conn)
      defInfo +=(id, spnl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"Port_list vector ${typeInfo}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        typeInfo.getScalarInitVal(valType, expOption)(defInfo)
      }
      val port = Port(id, valType, initVal, mode, conn)
      defInfo +=(id, port)
    }
  }

  def genISignal(id: String, sti: VSubtypeInd, signalKind: SignalKind.Ty): Unit = {
    val valType = sti.getSimpleName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo._guessListInitVals(valType)
      val spnl = SPnl(id, initVals, signalKind)
      defInfo +=(id, spnl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"signalDecl vector ${sti}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        fillInDefaultScalaVal(valType)
      }
      val signal = Signal(id, valType, initVal, signalKind)
      defInfo +=(id, signal)
    }
  }

}
