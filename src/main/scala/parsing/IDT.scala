package parsing

import parsing.V2IUtils.VHDLize

/////////////////////////////////////////////////////////////////////////////////

case class Variable(id: String, valType: String, iExp: IExp) extends IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"variable\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${id}]"

  /**
    * it gets **current** level name, not recursively
    */
  override def getId = id
}

sealed abstract class Vl extends IDef {

  override def toString = this match {
    case Vl_v(iVariable) => s"(vl_v ${iVariable})"
    case Vnl(id, vlList) => s"(vnl ('''', ${vlList.mkString("[\n ", ",\n ", "\n]")}))"
  }

  def as_list: String = this match {
    case Vl_v(iVariable) => iVariable.id
    case Vnl(id, _) => s"(vlist_of_v ${id})"
  }

  def as_definition: String = this match {
    /// "variable", this case should never be called
    case Vl_v(v) => v.as_definition
    /// becomes "vl"
    case Vnl(id, _) => {
      s"""definition ${id}:: \"vl\" where
          |\"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  override def getId = this match {
    case Vl_v(v) => v.id
    case Vnl(id, vlList) => id
  }

  /**
    * it may return a "variable" or a "vl" (which is vnl-generated)
    */
  def get(nList: List[String]): Option[IDef] = {
    def aux(l: List[String], cur: Option[Vl]): Option[Vl] = l match {
      case h :: t => cur match {
        case Some(vl) => vl match {
          // vl_v contains
          case vl_v: Vl_v => Some(vl)
          // vnl not sure, recursive
          case vnl: Vnl => aux(t, vnl.vlList.find(_.getId == h))
        }
        // no
        case None => None
      }
      // no more recursive
      case Nil => cur
    }
    aux(nList, Some(this)) match {
      case Some(Vl_v(v)) => Option(v)
      case other => other
    }
  }
}

case class Vl_v(iVariable: Variable) extends Vl


case class Vnl(id: String, vlList: List[Vl]) extends Vl

object Vnl {
  //  FIXME: this is TOO specific!
  // TODO: currently valType is the EXACTLY type from VHDL without prefix!!!
  def apply(id: String, dataList: List[IData])(implicit s: DummyImplicit): Vnl = {
    val vlList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      Vl_v(Variable(itemId, data.valType, data.initVal))
    }
    Vnl(id, vlList)
  }
}

/////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////

trait IDef {
  def as_definition: String

  def as_list: String

  def getId: IdTy
}

sealed abstract class SPl extends IDef {

  override def toString = this match {
    case SPl_s(s) => s"(spl_s ${s})"
    case SPl_p(p) => s"(spl_p ${p})"
    case SPnl(id, splList) => s"(spnl ('''', ${splList.mkString("[\n ", ",\n ", "\n]")}))"
  }

  def as_definition: String = this match {
    // this case should never be called
    case SPl_s(s) => s.as_definition
    // this case should never be called
    case SPl_p(p) => p.as_definition
    case SPnl(id, splList) => {
      s"""definition ${id}:: \"spl\" where
          | \"${id} ≡ ${toString}\"""".stripMargin
    }
  }

  def as_list = this match {
    case SPl_s(s) => s.id
    case SPl_p(p) => p.id
    case SPnl(id, _) => s"(splist_of_spl ${id})"
  }

  def getId = this match {
    case SPl_s(s) => s.getId
    case SPl_p(p) => p.getId
    case SPnl(id, splList) => id
  }

  def get(nList: List[String]): Option[IDef] = {
    def aux(l: List[String], cur: Option[SPl]): Option[SPl] = l match {
      case h :: t => cur match {
        case Some(spl) => spl match {
          case _: SPl_s => Some(spl)
          case _: SPl_p => Some(spl)
          case spnl: SPnl => aux(t, spnl.splList.find(_.getId == h))
        }
        case None => None
      }
      case Nil => cur
    }
    aux(nList, Some(this)) match {
      case Some(SPl_s(spl_s)) => Some(spl_s)
      case Some(SPl_p(spl_p)) => Some(spl_p)
      case other => other
    }
  }

}

case class SPl_s(iSignal: Signal) extends SPl

case class SPl_p(iPortDef: Port) extends SPl

case class SPnl(id: String, splList: List[SPl]) extends SPl

object SPnl {
  def apply(id: String, dataList: List[IData], signalKind: SignalKind.Ty): SPnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      SPl_s(Signal(itemId, data.valType, data.initVal, signalKind))
    }
    SPnl(id, splList)
  }

  def apply(id: String, dataList: List[IData], mode: PortMode.Ty, conn: PortConn.Ty): SPnl = {
    val splList = for {
      data <- dataList
    } yield {
      val itemId = s"${id}_${data.itemId}"
      SPl_p(Port(itemId, data.valType, data.initVal, mode, conn))
    }
    SPnl(id, splList)
  }
}

//////////////////////////////////////////////////////////////////////////////////

object SignalKind extends Enumeration {
  type Ty = Value
  val register, bus = Value
}

case class Signal(id: String, valType: String, iExp: IExp, signalKind: SignalKind.Ty) extends IDef {
  override def toString = s"""(''${id}'', ${VHDLize(valType)}, ${signalKind}, ${iExp})"""

  override def as_definition: String = {
    s"""definition ${id}:: \"signal\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"[${SP_s(this)}]"

  override def getId = id
}

object PortMode extends Enumeration {
  type Ty = Value
  val in = Value("mode_in")
  val out = Value("mode_out")
  val inout = Value("mode_inout")
  val linkage = Value("mode_linkage")
}

object PortConn extends Enumeration {
  type Ty = Value
  val connected, unconnected = Value
}

case class Port(id: String, valType: String, iExp: IExp, mode: PortMode.Ty, conn: PortConn.Ty) extends IDef {
  override def toString = s"(''${id}'', ${VHDLize(valType)}, ${mode}, ${conn}, ${iExp})"

  override def as_definition: String = {
    s"""definition ${id}:: \"port\" where
        | \"${id} ≡ ${toString}\"""".stripMargin
  }

  override def as_list: String = s"${SP_p(this)}"

  override def getId = id
}

/////////////////////////////////////////////////////////////////////////////////

sealed abstract class SigPrt {
  override def toString = this match {
    case SP_s(s) => s"(sp_s ${s.id})"
    case SP_p(p) => s"(sp_p ${p.id})"
  }
}

// "up cast: Signal=>SigPrt"
case class SP_s(iSignal: Signal) extends SigPrt

// "up cast: Port=>SigPrt"
case class SP_p(iPort: Port) extends SigPrt

/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////