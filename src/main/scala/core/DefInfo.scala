package core

import org.slf4j.LoggerFactory

import scala.collection.mutable

final class DefInfo(defInfo: Option[DefInfo]) {

  val logger = LoggerFactory.getLogger(getClass)

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

  type V_PTy = Variable
  type Vnl_PTy = Vnl
  type S_PTy = Signal
  type P_PTy = Port
  type SPnl_PTy = SPnl

  val v_raw = mutable.ListBuffer.empty[Variable]
  val vnl_raw = mutable.ListBuffer.empty[Vnl_PTy]
  val s_raw = mutable.ListBuffer.empty[S_PTy]
  val p_raw = mutable.ListBuffer.empty[P_PTy]
  val spnl_raw = mutable.ListBuffer.empty[SPnl_PTy]

  //  for Env
  val v_map = mutable.Map.empty[IdTy, Variable]
  val s_map = mutable.Map.empty[IdTy, Signal]
  val p_map = mutable.Map.empty[IdTy, Port]

  /**
    * get def, which means getting
    * 1. "variable"(v_raw), "signal"(s_raw), "port"(p_raw)
    * 2. (1) from spnl_raw for "signal", "port", "spnl"
    * (2) from v_raw for "variable", "vnl"
    * NOTE: this could be implemented by flattening the map beforehand
    * NOTE: isar representation of "top level scalar" and "inside scalar" should be different!
    */

  def getDef(n: String): IDef = getDefOpt(List(n)).getOrElse(throw VIErrorMsg(s"${n}"))

  def getDef(sn: VSelectedName): IDef = {
    val nList = sn.suffixList.scanLeft(sn.id)((acc, cur) => s"${acc}_${cur.s}")
    getDefOpt(nList).getOrElse(throw VIErrorMsg(s"${sn}"))
  }

  def getSPDef(sn: VSelectedName): SP_IDef = {
    val nList = sn.suffixList.scanLeft(sn.id)((acc, cur) => s"${acc}_${cur.s}")
    getSPDefOpt(nList).getOrElse(throw VIErrorMsg(s"${sn}"))
  }

  def getVDef(sn: VSelectedName): V_IDef = {
    val nList = sn.suffixList.scanLeft(sn.id)((acc, cur) => s"${acc}_${cur.s}")
    getVDefOpt(nList).getOrElse(throw VIErrorMsg(s"${sn}"))
  }

  def getSPDefOpt(nList: List[String]): Option[SP_IDef] = {
    val h = nList.head
    (s_raw.find(_.id == h), p_raw.find(_.id == h)) match {
      case (None, None) => spnl_raw.find(_.id == h) match {
        case Some(spl) => spl.get(nList.tail)
        case None => None
      }
      case (s@Some(ss), None) => s
      case (None, p@Some(sp)) => p
      case (_, _) => throw VIErrorMsg(s"${nList}")
    }
  }

  def getVDefOpt(nList: List[String]): Option[V_IDef] = {
    val h = nList.head
    v_raw.find(_.id == h) match {
      case v@Some(sv) => v
      case None => vnl_raw.find(_.id == h) match {
        case Some(vl) => vl.get(nList.tail)
        case None => None
      }
    }
  }

  def getDefOpt(nList: List[String]): Option[IDef] = getSPDefOpt(nList).orElse(getVDefOpt(nList))

  def vnl_flatten(vnl: Vnl): List[V_PTy] = vnl.vlList flatMap {
    case vnl: Vnl => vnl_flatten(vnl)
    case Vl_v(v) => List(v)
  }

  def spnl_flatten(spnl: SPnl): (List[S_PTy], List[P_PTy]) = {
    def aux(sPnl: SPnl): List[(S_PTy, P_PTy)] = spnl.splList flatMap {
      case spnl: SPnl => aux(spnl)
      case SPl_s(s) => List((s, null))
      case SPl_p(p) => List((null, p))
    }
    val (signalList, portList) = aux(spnl).unzip
    (signalList.filter(_ != null), portList.filter(_ != null))
  }


  defInfo match {
    case Some(di) => {
      v_map ++= di.v_map
      s_map ++= di.s_map
      p_map ++= di.p_map
    }
    case None => {
      p_map += {
        //    TODO currently rewrite "clk" to "p_clk" in vhd file
        //    TODO ideally definition should be parsed from vhd file
        //    TODO definition should not exist in isar file
        val id = "p_clk"
        val iVariable = IConstS("val_c", "CHR ''0''")
        val exp_con = IExp_con("std_ulogic", iVariable)
        id -> Port(id, "std_ulogic", exp_con, PortMode.withName("mode_in"), PortConn.connected)
      }
    }
  }

  def +=(id: String, d: Variable): Unit = {
    v_raw += d
    v_map += (id -> d)
  }

  def +=(id: String, d: Vnl): Unit = {
    vnl_raw += d
    val vList = vnl_flatten(d)
    v_map ++= vList.map(_.id).zip(vList)
  }

  def +=(id: String, d: Signal): Unit = {
    s_raw += d
    s_map += (id -> d)
  }

  def +=(id: String, d: Port): Unit = {
    p_raw += d
    p_map += (id -> d)
  }

  def +=(id: String, d: SPnl): Unit = {
    spnl_raw += d
    val (sl, pl) = spnl_flatten(d)
    s_map ++= sl.map(_.id).zip(sl)
    p_map ++= pl.map(_.id).zip(pl)
  }

  def dumpTables() = {
    logger.info(s"${
      v_map.mkString("\n")
    }")
    logger.info(s"${
      s_map.mkString("\n")
    }")
    logger.info(s"${
      p_map.mkString("\n")
    }")
  }

  override def toString = {
    val defList = for {
      defs <- List(v_raw, vnl_raw, s_raw, p_raw, spnl_raw)
      d <- defs.toList
    } yield d.as_definition
    defList.mkString("\n\n")
  }

}
