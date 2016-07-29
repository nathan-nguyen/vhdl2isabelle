package parsing

import parsing.V2IUtils.VHDLize

import scala.collection.mutable

class DefInfo {

  //  TODO perhaps need preserving order for each map

  val vs = mutable.Map.empty[String, IVarScalarDef]
  val vl = mutable.Map.empty[String, IVarListDef]
  val ss = mutable.Map.empty[String, ISignalScalarDef]
  val sl = mutable.Map.empty[String, ISignalListDef]
  val ps = mutable.Map.empty[String, IPortScalarDef]
  val pl = mutable.Map.empty[String, IPortListDef]

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

/////////////////////////////////////////////////////////////////////////////////

abstract class IDef {
  def repr: String
}

sealed abstract class IVarDef extends IDef

case class IVarScalarDef(id: String, valType: String, iExp: IExp) extends IVarDef {
  override def toString = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""vl_v (''${id}'', ${VHDLize(valType)}, ${iExp})"""

  def repr: String = id
}

case class IVarListDef(id: String, iVals: List[IScalarOrVecIval]) extends IVarDef {
  override def toString = {
    val varDefs = for {
      iVal <- iVals
    } yield IVarScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal)

    val itemsRepr = varDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"vl\" where
        |\"${id} ≡ vnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  override def repr: String = s"(vlist_of_vl ${id})"
}

//////////////////////////////////////////////////////////////////////////////////

sealed abstract class ISigPrt extends IDef {
}

sealed abstract class IPortDef extends ISigPrt

case class IPortScalarDef(id: String, valType: String, iExp: IExp, mode: String, conn: String = "connected") extends IPortDef {
  override def toString = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_p (''${id}'', ${VHDLize(valType)}, mode_${mode}, ${conn}, ${iExp})"""

  override def repr: String = s"(sp_p ${id})"
}

case class IPortListDef(id: String, iVals: List[IScalarOrVecIval], mode: String, conn: String = "connected") extends IPortDef {
  override def toString = {
    val portDefs = for {
      iVal <- iVals
    } yield IPortScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal, mode, conn)
    val itemsRepr = portDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"spl\" where
        |\"${id} ≡ spnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  override def repr: String = s"(sp_p ${id})"
}

sealed abstract class ISignalDef extends ISigPrt

case class ISignalScalarDef(id: String, valType: String, iExp: IExp, signalKind: String = "register") extends ISignalDef {
  override def toString = {
    s"""definition ${id}:: \"signal\" where
        | \"${id} ≡ (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})\"""".stripMargin
  }

  def asItem = s"""spl_s (''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def repr: String = s"(sp_s ${id})"
}

case class ISignalListDef(id: String, iVals: List[IScalarOrVecIval], signalKind: String = "register") extends ISignalDef {
  override def toString = {
    val signalDefs = for {
      iVal <- iVals
    } yield ISignalScalarDef(s"${id}_${iVal.itemId}", iVal.valType, iVal.initVal, signalKind)
    val itemsRepr = signalDefs.map(_.asItem).mkString("[\n ", ",\n ", "\n]")
    s"""definition ${id}:: \"spl\" where
        |\"${id} ≡ spnl ('''', ${itemsRepr})\"
     """.stripMargin
  }

  override def repr: String = s"(sp_s ${id})"
}
