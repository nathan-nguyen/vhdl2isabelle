package parsing

import org.slf4j.LoggerFactory
import parsing.V2IUtils._

import scala.collection.mutable

trait Keeper {

  protected def vInfo: Option[VInfo]

  protected def vInfo_=(info: Option[VInfo]): Unit

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new TypeInfo(vInfo.map(_.typeInfo))

  val cStateInfo = new CStatInfo

  val definedEntities = mutable.ArrayBuffer.empty[String]

  val logger = LoggerFactory.getLogger(classOf[TVisitor])

  def genIVariable(id: String, expOption: Option[VExp], sti: VSubtypeInd): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)
      val vl = IVarListDef(id, initVals)
      defInfo +=(id, vl)
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
      val vs = IVarScalarDef(id, valType, initVal)
      defInfo +=(id, vs)
    }
  }

  def genIPort(id: String, expOption: Option[VExp], sti: VSubtypeInd, mode: String, conn: String): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo.getListInitVals(valType, expOption)
      val pl = IPortListDef(id, initVals, mode)
      defInfo +=(id, pl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"Port_list vector ${typeInfo}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        typeInfo.getScalarInitVal(valType, expOption)
      }
      val ps = IPortScalarDef(id, valType, initVal, mode)
      defInfo +=(id, ps)
    }
  }

  def genISignal(id: String, sti: VSubtypeInd, signalKind: String): Unit = {
    val valType = sti.selectedName
    if (typeInfo.isListType(valType)) {
      val initVals = typeInfo._guessListInitVals(valType)
      val sl = ISignalListDef(id, initVals, signalKind)
      defInfo +=(id, sl)
    } else {
      val initVal = if (typeInfo.isVectorType(valType)) {
        val range = sti.getRange.getOrElse(defaultRange(s"signalDecl vector ${sti}"))
        typeInfo._guessVectorInitVal(valType, range)
      } else {
        typeInfo._guessScalarInitVal(valType)
      }
      val ss = ISignalScalarDef(id, valType, initVal, signalKind)
      defInfo +=(id, ss)
    }
  }

}
