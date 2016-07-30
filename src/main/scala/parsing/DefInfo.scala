package parsing

import scala.collection.mutable

final class DefInfo(defInfo: Option[DefInfo]) {

  //  TODO perhaps need preserving order for each map

  val vs = mutable.Map.empty[String, IVarScalarDef]
  val vl = mutable.Map.empty[String, IVarListDef]
  val ss = mutable.Map.empty[String, ISignalScalarDef]
  val sl = mutable.Map.empty[String, ISignalListDef]
  val ps = mutable.Map.empty[String, IPortScalarDef]
  val pl = mutable.Map.empty[String, IPortListDef]

  defInfo match {
    case Some(di) => {
      vs ++= di.vs
      vl ++= di.vl
      ss ++= di.ss
      sl ++= di.sl
      ps ++= di.ps
      pl ++= di.pl
    }
    case None => {
      ps += {
        //    TODO currently rewrite "clk" to "p_clk" in vhd file
        //    TODO ideally definition should be parsed from vhd file
        //    TODO definition should not exist in isar file
        val id = "p_clk"
        val iVariable = IVariable("val_c", "CHR ''0''")
        val exp_con = IExp_con("std_ulogic", iVariable)
        id -> IPortScalarDef(id, "std_ulogic", exp_con, "in", "connected")
      }
    }
  }

  def +=(id: String, d: IVarScalarDef) = vs += (id -> d)

  def +=(id: String, d: IVarListDef) = vl += (id -> d)

  def +=(id: String, d: ISignalScalarDef) = ss += (id -> d)

  def +=(id: String, d: ISignalListDef) = sl += (id -> d)

  def +=(id: String, d: IPortScalarDef) = ps += (id -> d)

  def +=(id: String, d: IPortListDef) = pl += (id -> d)

  override def toString = {
    val defList: List[IDef] = for {
      defs <- List(vs, vl, ss, sl, ps, pl).map(_.values)
      d <- defs
    } yield d
    defList.mkString("\n\n")
  }

}
