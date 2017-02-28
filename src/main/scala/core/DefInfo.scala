package core

import core.isabellesyntax._
import core.vhdlsyntax._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Hongxu Chen.
  */
final class DefInfo(defInfo: Option[DefInfo]) {

  /**
    * two strategies can be applied for table lookup:
    * (1) flatten all table and simply check name
    * (2) on-the-fly
    * currently choose (2)
    */

  /**
    * there is only 5 kinds of definitions useful for table lookup
    * variable (scalar), vnl (record)
    * signal (scalar), port (scalar), spnl (record)
    */

  type Vnl_PTy = IVl_Vnl

  val v_raw = mutable.ListBuffer.empty[IVariable]
  val vnl_raw = mutable.ListBuffer.empty[Vnl_PTy]
  val s_raw = mutable.ListBuffer.empty[Signal]
  val p_raw = mutable.ListBuffer.empty[Port]
  val spnl_raw = mutable.ListBuffer.empty[ISpl_Spnl]

  //  for Env
  val v_map = mutable.Map.empty[String, IVariable]
  val s_map = mutable.Map.empty[String, Signal]
  val p_map = mutable.Map.empty[String, Port]

  /**
    * get def, which means getting
    * 1. "variable"(v_raw), "signal"(s_raw), "port"(p_raw)
    * 2. (1) from spnl_raw for "signal", "port", "spnl"
    *    (2) from v_raw for "variable", "vnl"
    * NOTE: This could be implemented by flattening the map beforehand
    * NOTE: Isabelle representation of "top level scalar" and "inside scalar" should be different!
    */

  def getDef(n: String): IDef = getDefOpt(List(n)).getOrElse(handler(s"${n}"))

  def containsDef(n: String) = getDefOpt(List(n)) match {
    case Some(_) => true
    case None => false
  }

  def getDef(selectedName: VSelectedName): IDef = {
    val nList = selectedName.suffixList.scanLeft(selectedName.id)((acc, cur) => s"${acc}_${cur.s}")
    getDefOpt(nList).getOrElse(handler(s"${selectedName}"))
  }

  def getSPDef(selectedName: VSelectedName): SP_IDef = {
    val nList = selectedName.suffixList.scanLeft(selectedName.id)((acc, cur) => s"${acc}_${cur.s}")
    getSPDefOpt(nList).getOrElse(handler(s"${selectedName}"))
  }

  def getVDef(selectedName: VSelectedName): V_IDef = {
    val nList = selectedName.suffixList.scanLeft(selectedName.id)((acc, cur) => s"${acc}_${cur.s}")
    getVDefOpt(nList).getOrElse(handler(s"${selectedName}"))
  }

  def getSPDefOpt(h: String, nList: List[String]): Option[SP_IDef] = {
    (s_raw.find(_.id == h), p_raw.find(_.id == h)) match {
      case (None, None) => spnl_raw.find(_.id == h) match {
        case Some(spl) => spl.get("",nList.tail)
        case None => None
      }
      case (s@Some(ss), None) => s
      case (None, p@Some(sp)) => p
      case (_, _) => handler(s"${nList}")
    }
  }

  def getSPDefOpt(nList: List[String]): Option[SP_IDef] = {
    val h = nList.head
    if (IdentifierMap.isParsingSubprogram){
      val subprogramName = s"${IdentifierMap.subprogramName}_"
      val h_rename = s"${subprogramName}${h}"
      (s_raw.find(_.id == h_rename), p_raw.find(_.id == h_rename)) match {
        case (s@Some(ss), None) => s
        case (None, p@Some(sp)) => p
        case (None, None) => spnl_raw.find(_.id == h_rename) match {
          case Some(spl) => spl.get(subprogramName, nList.tail)
          case None => getSPDefOpt(h, nList)
        }
        case (_, _) => handler(s"${nList}")
      }
    } else {
      getSPDefOpt(h, nList)
    }
  }

  def getVDefOpt(h: String, nList: List[String]): Option[V_IDef] = {
    v_raw.find(_.name == h) match {
      case v@Some(sv) => v
      case None => vnl_raw.find(_.id == h) match {
        case Some(vl) => vl.get("",nList.tail)
        case None => None
      }
    }
  }

  def getVDefOpt(nList: List[String]): Option[V_IDef] = {
    /** [TN] This hack allows looking for local variable definition in subprogram call
      * Because all the local variables are renamed: subprogram_name + "_" + local_variable_name
      * This function might not work with nested vl because the only used values are head and tail
    */
    val h = nList.head
    if (IdentifierMap.isParsingSubprogram){
      val subprogramName =  s"${IdentifierMap.subprogramName}_"
      val h_rename = s"${subprogramName}${h}"
      v_raw.find(_.name == h_rename) match {
        case v@Some(sv) => v
        case None => vnl_raw.find(_.id == h_rename) match {
          case Some(vl) => vl.get(subprogramName, nList.tail)
          case None => getVDefOpt(h, nList)
        }
      }
    } else getVDefOpt(h, nList)
  }

  def getDefOpt(nList: List[String]): Option[IDef] = getVDefOpt(nList).orElse(getSPDefOpt(nList))

  def vnl_flatten(vnl: IVl_Vnl): List[IVariable] = vnl.vlList flatMap {
    case iVl_Vnl: IVl_Vnl => vnl_flatten(iVl_Vnl)
    case IVl_Vl_v(v) => List(v)
    case _ => ???
  }

  def iSpl_Spnl_flatten(iSpl_Spnl: ISpl_Spnl): (List[Signal], List[Port]) = {
    def aux(sPnl: ISpl_Spnl): List[(Signal, Port)] = sPnl.splList flatMap {
      case ISPl_s(s) => List((s, null))
      case ISpl_p(p) => List((null, p))
      case iSpl_Spnl: ISpl_Spnl => aux(iSpl_Spnl)
    }
    val (signalList, portList) = aux(iSpl_Spnl).unzip
    (signalList.filter(_ != null), portList.filter(_ != null))
  }


  defInfo match {
    case Some(di) => {
      v_raw ++= di.v_raw
      v_map ++= di.v_map
      s_raw ++= di.s_raw
      s_map ++= di.s_map
      p_raw ++= di.p_raw
      p_map ++= di.p_map
    }
    case None =>
  }

  def +=(id: String, d: IVariable): Unit = {
    v_raw += d
    v_map += (id -> d)
  }

  def +=(id: String, d: IVl_Vnl): Unit = {
    vnl_raw += d
    val vList = vnl_flatten(d)
    v_map ++= vList.map(_.name).zip(vList)
  }

  def +=(id: String, d: Signal): Unit = {
    s_raw += d
    s_map += (id -> d)
  }

  def +=(id: String, d: Port): Unit = {
    p_raw += d
    p_map += (id -> d)
  }

  def +=(id: String, iSpl_Spnl: ISpl_Spnl): Unit = {
    spnl_raw += iSpl_Spnl
    val (sl, pl) = iSpl_Spnl_flatten(iSpl_Spnl)
    s_map ++= sl.map(_.id).zip(sl)
    p_map ++= pl.map(_.id).zip(pl)
  }

  def dumpTables() = {
    logger.info(s"${v_map.mkString("\n")}")
    logger.info(s"${s_map.mkString("\n")}")
    logger.info(s"${p_map.mkString("\n")}")
  }

  override def toString = {
    val defList = for {
      defs <- List(v_raw, vnl_raw, s_raw, p_raw, spnl_raw)
      d <- defs.toList
    } yield d.as_definition
    defList.mkString("\n\n")
  }
}
