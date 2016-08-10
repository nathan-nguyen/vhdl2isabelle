package core

import org.slf4j.LoggerFactory
import sg.edu.ntu.hchen.VHDLParser.{Identifier_listContext, Subtype_indicationContext}

import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class VInfo(typeInfo: TypeInfo, defInfo: DefInfo)

object V2IUtils {

  val defaultId = ""

  val logger = LoggerFactory.getLogger(this.getClass)

  type RangeTy = (String, String, String)

  def getIdList(ctx: Identifier_listContext): List[String] = {
    val ids = for {
      id <- ctx.identifier()
    } yield id.getText.toLowerCase
    ids.toList
  }

  def fillInDefaultScalaVal(valType: String): IExp_con = valType match {
    case "integer" => IExp_con(valType, IConstS("val_i", "0"))
    case "real" => IExp_con(valType, IConstS("val_r", "0.0"))
    case "character" => IExp_con(valType, IConstS("val_c", "(CHR ''0'')"))
    case "boolean" => IExp_con(valType, IConstS("val_b", "True"))
    case "std_ulogic" => IExp_con(valType, IConstS("val_c", "(CHR ''0'')"))
    case "std_logic" => IExp_con(valType, IConstS("val_c", "(CHR ''0'')"))
    case _ => defaultExpCon(s"scalar unknown ${valType}")
  }

  def selectedNameFromSubtypeInd(ctx: Subtype_indicationContext): VSelectedName = {
    val names = for {
      name <- ctx.selected_name()
    } yield VSelectedName(name)
    val head = names.head
    require(head.suffixList.isEmpty, "selectedNameFromSubtypeInd")
    head
  }

  def defaultTargetName(msg: String): String = {
    logger.warn(s"defaultTargetName: ${msg}")
    s"===${unknownString}==="
  }

  def defaultRange(msg: String): RangeTy = {
    logger.warn(s"defaultRange: ${msg}")
    (unknownString, unknownString, unknownString)
  }

  def defaultExpCon(msg: String): IExp_con = {
    logger.warn(s"defaultExpCon: ${msg}")
    throw VIErrorMsg(msg)
  }

  def VHDLize(vhdlType: String) = s"vhdl_${vhdlType}"

  // TODO
  def def__v_clhs(idef: V_IDef, range: Option[Discrete_range], sn:VSelectedName): V_clhs = idef match {
    case variable: Variable => {
      val v_lhs = range match {
        case None => Lhs_v(variable, sn)
        case Some(discrete_range) => Lhs_va(variable, discrete_range, sn)
      }
      Clhs_v(v_lhs)
    }
    case vl: Vl => Clhs_vr(vl)
    case _ => throw VIErrorMsg(s"${idef}")
  }

  // TODO
  def def__sp_clhs(idef: SP_IDef, range: Option[Discrete_range], sn:VSelectedName): SP_clhs = idef match {
    case signal: Signal => {
      val sp_s = SP_s(signal, sn)
      val sp_lhs = range match {
        case None => Lhs_s(sp_s, sn)
        case Some(discrete_range) => Lhs_sa(sp_s, discrete_range, sn)
      }
      Clhs_sp(sp_lhs)
    }
    case port: Port => {
      val sp_p = SP_p(port, sn)
      val sp_lhs = range match {
        case None => Lhs_s(sp_p, sn)
        case Some(discrete_range) => Lhs_sa(sp_p, discrete_range, sn)
      }
      Clhs_sp(sp_lhs)
    }
    case spl: SPl => Clhs_spr(spl)
    case _ => throw VIErrorMsg(s"${idef}")
  }

}
